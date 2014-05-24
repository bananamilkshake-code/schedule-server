#!/usr/bin/ruby

### Скрипт, иммитирующий работу пользователей для приложения Open Schedule.
### Испльзовался для проведения нагрузочного тестирования серверной
### части приложения.

require 'socket'
require 'thread'
require 'bindata'

# Генерирует случайную последовательность символов длиной len.
def generate_str len
	return (0..len).map { ('a'..'z').to_a.sample }.join
end

def generate_table_data
	name = generate_str 5
	description = generate_str 5
	return name, description
end

def generate_task_data
	name = generate_str 5
	description = generate_str 5
	# По сути без разницы, какое значение будет в этих полях, 
	# поэтому заполним их любыми значениями, которые могут быть
	# прочитаны как "yyyyMMdd" и "HHmm"
	start_date = "20001010"
	end_date = "20001010"
	start_time = "1120"
	end_time = "1230"
	period = rand(1..30)
	return name, description, start_date, end_date, start_time, end_time, period
end

###
### Пакет, который отправлякется с клиентского приложения.
###
class ClientPacket

	# Номера пакетов
	REGISTER = 0
	LOGIN = 1
	CREATE_TABLE = 2
	CREATE_TASK = 3
	TABLE_CHANGE = 4
	TASK_CHANGE = 5
	PERMISSION = 6
	COMMENTARY = 7

	# Маски пакетоа.
	MASK = [
		"S>A*S>A*",			# REGISTER
		"S>A*S>A*",			# LOGIN
		"L>Q>S>A*S>A*",		# CREATE_TABLE
		"L>L>Q>S>A*S>A*A*A*A*",# CREATE_TASK
		"L>Q>S>A*S>A*",		# TABLE_CHANGE
		"L>L>Q>S>A*S>A*A*A*A*",# TASK_CHANGE
		"L>L>C",			# PERMISSION
		"L>L>Q>S>A*"		# COMMENTARY
	]

	def initialize packet_type
		@packet_type = packet_type
	end

	# Добавление данных в пакет. Для реализации цеполчки вызовов,
	# метод возвращает указатель на self.
	def add data
		@data = Array.new if @data.nil?
		@data << data.bytesize * 8 if data.is_a? String
		@data << data
		return self
	end

	# Преобразуем данные к определенному формаату. Добавляем
	# в начало номер пакета и размер пакета.
	def pack
		packet = @data.pack(MASK[@packet_type])
		attributes = [@packet_type, packet.bytesize * 8]
		return attributes.pack("Cs>") + packet
	end
end

###
### Серверные пакеты, приходящие на клиентское приложение
###

REGISTER = 0
LOGIN = 1
GLOBAL_TABLE_ID = 2
GLOBAL_TASK_ID = 3

class ServerPacket < BinData::Record
	endian :big
	uint8 :type
	uint16 :len
end

class RegisterPacker < ServerPacket
	uint8 :status
end

class LoginPacket < ServerPacket
	uint8 :status
	uint32 :user_id, :onlyif => :succeeded?

	def succeeded?
		return status.zero?
	end
end

class GlobalTablePacket < ServerPacket
	uint32 :table_id
	uint32 :global_id
end

class GlobalTaskPacket < ServerPacket
	uint32 :task_id
	uint32 :global_id
	uint32 :global_table_id
end

###
### Класс, имитирующий работу клиентской части
### приложения. Отвечает за формирование и отправку 
### пакетов на сервер.
###

MAX_PACKET_SIZE = 60000

class Client
	def initialize params
		@socket = TCPSocket.open params.host, params.port
		@request = nil
		@responce = nil

		@timeout = params.timeout
	end

	def run
		listen
		make @timeout
		@responce.join
		@request.join
	end

	def listen
		@request = Thread.new do
			loop do
				packet = @socket.recv MAX_PACKET_SIZE
				parse packet
			end
		end
	end

	def make timeout
		@responce = Thread.new do
			loop do
				make_something
				sleep timeout
			end
		end
	end

	def send packet
		@socket.write packet.pack
	end

	def parse packet
		type = packet.bytes.to_a[0][0]
		case type
		when REGISTER
			do_register packet
		when LOGIN
			do_login packet
		when GLOBAL_TABLE_ID
			do_global_table packet
		when GLOBAL_TASK_ID
			do_global_task packet
		end
	end

	# Регистрация нового клиента. Генерируем случайные логин и пароль и
	# отправляем на сервер.
	def register
		@name = generate_str 15
		@password = generate_str 15
		send ClientPacket.new(ClientPacket::REGISTER).add(@name).add(@password)
	end

	def login
		send ClientPacket.new(ClientPacket::LOGIN).add(@name).add(@password)
	end

	def _make_table table_id, name, description
		time = Time.now.to_i
		send ClientPacket.new(ClientPacket::CREATE_TABLE).add(table_id).add(time)
				.add(name).add(description)
	end

	def _make_task global_table_id, new_task_id, name, description, start_date, end_date, start_time, end_time, period
		time = Time.now.to_i
		send ClientPacket.new(ClientPacket::CREATE_TASK).add(new_task_id).add(global_table_id).add(time)
			.add(name).add(description).add(start_date).add(end_time).add(start_time).add(end_time)
	end

	def _make_comment global_table_id, global_task_id, text
		time = Time.now.to_i
		send ClientPacket.new(ClientPacket::COMMENTARY).add(global_table_id).ad(global_task_id)
			.add(time).add(text)
	end

	def _change_table global_table_id, name, description
		time = Time.now.to_i
		send ClientPacket.new(ClientPacket::TABLE_CHANGE).add(global_table_id).add(time)
			.add(name).add(description)
	end

	def _change_task global_table_id, global_task_id, name, description, start_date, end_date, start_time, end_time, period
		time = Time.now.to_i
		send ClientPacket.new(ClientPacket::TASK_CHANGE).add(global_task_id).add(global_table_id).add(time)
			.add(name).add(description).add(start_date).add(end_time).add(start_time).add(end_time)
	end

	def _change_permission global_table_id, user_id, permission
		send ClientPacket.new(ClientPacket::PERMISSION).add(user_id).add(global_table_id).add(permission)
	end

	def do_register packet
		register_packet = RegisterPacker.read packet
	end

	def do_login packet
		login_packet = LoginPacket.read packet
		if login_packet.succeeded? then 
			@id = login_packet.user_id 
		else
			register
		end
	end

	def do_global_table packet
		global_table = GlobalTablePacket.read packet
		update_global_table global_table.table_id, global_table.global_id
	end

	def do_global_task packet
		global_task = GlobalTaskPacket.read packet
		update_global_task global_task.task_id global_task.global_id, global_task.global_table_id
	end

	public :run
	protected :register, :login, :_make_table, :_make_task, :_make_comment,
		:_change_table, :_change_task, :_change_permission
	private :listen, :make, :send, :parse,
		:do_register, :do_login
end

MAX_PERMISSION_TYPE = 2

###
### Имитация логики работы клиентского приложения. Совершает некоторые действия
### через заданныей промежуток времени.
###
class User < Client
	def initialize params
		super params

		# Номер последней созданной таблицы.
		@table_id = 0

		# Ассоциация номеров таблиц с глобальной базой данных.
		@global_table_id = Array.new

		# Номера последних заданий для каждой таблицы.
		@task_id = Array.new

		# Ассоциация номером заданий с заданиями в глобпальной базе данных.
		@global_task_id = Array.new
	end

	def make_something
		if @id.nil? then
			auth
		else
			[method(:make_table), method(:make_task), method(:make_comment), 
				method(:change_table), method(:change_task), method(:change_permission)].sample.call
		end
	end

	def auth
		@name.nil? ? register : login
	end

	def make_table
		new_table_id = generate_new_table
		name, description = generate_table_data
		_make_table new_table_id, name, description
	end

	def make_task
		table_id, global_table_id = get_table_id
		return if table_id.nil? or @task_id[table_id].nil?
		new_task_id = generate_new_task table_id
		name, description, start_date, end_date, start_time, end_time, period = generate_task_data
		_make_task global_table_id, new_task_id, name, description, start_date, end_date, start_time, end_time, period
	end

	def make_comment
		table_id, global_table_id = get_table_id
		task_id, global_task_id = get_task_id table_id
		return if task_id.nil?
		text = generate_str 20
		_make_comment global_table_id, global_task_id, text
	end

	def change_table
		table_id, global_table_id = get_table_id
		return if table_id.nil?
		name, description = generate_table_data
		_change_table global_table_id, name, description
	end

	def change_task
		table_id, global_table_id = get_table_id
		task_id, global_task_id = get_task_id table_id
		return if task_id.nil?
		name, description, start_date, end_date, start_time, end_time, period = generate_task_data
		_change_task global_table_id, global_task_id, name, description, start_date, end_date, start_time, end_time, period
	end

	def change_permission
		table_id, global_table_id = get_table_id
		return if table_id.nil?
		user_id = generate_user_id
		permission = rand(MAX_PERMISSION_TYPE)
		_change_permission global_table_id, user_id, permission
	end

	def generate_new_table
		new_table_id = @table_id
		@table_id += 1
		@task_id[new_table_id] = 0
		@global_table_id[new_table_id] = 0
		return new_table_id
	end

	def generate_new_task table_id		
		new_task_id = @task_id[table_id]
		@task_id += 1
		@global_task_id[table_id] = Array.new if @global_task_id[table_id].nil?
		@global_task_id[table_id][new_task_id] = 0
		return new_task_id
	end

	def generate_user_id
		return rand(1..@id-1)
	end

	def get_table_id
		global_table_id = @global_table_id.select{|global_id| global_id.nonzero?}.sample
		return nil, nil if global_table_id.nil?
		table_id = @global_table_id.index global_table_id
		return table_id, global_table_id
	end

	def get_task_id table_id
		return nil, nil if table_id.nil? or @task_id[table_id].zero?
		global_task_id = @global_task_id[table_id].select{|global_id| global_id.nonzero?}.sample
		return nil, nil if global_task_id.nil?
		task_id = @global_task_id.index global_task_id
		return task_id, global_task_id
	end

	def update_global_table table_id, global_id
		@global_table_id[table_id] = global_id
	end

	def update_global_task task_id, global_id, global_table_id
		table_id = @global_table_id.index global_table_id
		@global_task_id[table_id][task_id] = global_id
	end

	protected :make_something, :update_global_table, :update_global_task
	private :generate_new_table, :generate_new_task, :generate_table_data, :generate_task_data, :get_table_id, :get_task_id, 
		:auth, :make_table, :make_task, :make_comment, :change_table, :change_task, :change_permission
end

Params = Struct.new(:host, :port, :threads_count, :timeout) do end
begin
	# В командной строке передаем адрес и порт, на котором запущен
	# сервер, количество потоков выполнения (количество пользователей),
	# и с какой частотой пользователи будут совершать действия.

	host = ARGV[0]
	port = ARGV[1].to_i
	threads_count = ARGV[2].to_i
	timeout = ARGV[3].to_i

	params = Params.new host, port, threads_count, timeout

	users_threads = []
	params.threads_count.times do
		users_threads << Thread.new() {
			User.new(params).run
		}
		Signal.trap("CLD") do Thread.kill user_threads.last end
	end

	users_threads.each do |user|
		user.join
	end
end