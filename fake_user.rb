#!/usr/bin/ruby

require 'socket'
require 'thread'
require 'bindata'

class ClientPacket
	REGISTER = 0
	LOGIN = 1
	CREATE_TABLE = 2
	CREATE_TASK = 3
	TABLE_CHANGE = 4
	TASK_CHANGE = 5
	PERMISSION = 6
	COMMENTARY = 7

	MASK = [
		"S>A*S>A*",				# REGISTER
		"S>A*S>A*",				# LOGIN
		"L>Q>S>A*S>A*",			# CREATE_TABLE
		"L>L>Q>S>A*S>A*A*A*A*A*",# CREATE_TASK
		"L>Q>S>A*S>A*",			# TABLE_CHANGE
		"L>L>Q>S>A*S>A*A*A*A*A*",# TASK_CHANGE
		"L>L>C",				# PERMISSION
		"L>L>Q>S>sA*"			# COMMENTARY
	]

	def initialize packet_type
		@packet_type = packet_type
	end

	def add data
		@data = Array.new if @data.nil?
		@data << data.bytesize * 8 if data.is_a? String
		@data << data
		return self
	end

	def pack
		packet = @data.pack(MASK[@packet_type])

		attributes = Array.new
		attributes << @packet_type
		attributes << packet.bytesize * 8

		return attributes.pack("Cs>") + packet
	end
end

REGISTER = 0
LOGIN = 1
GLOBAL_TABLE_ID = 2
GLOBAL_TASK_ID = 3
TABLE_CHANGE = 4
TASK_CHANGE = 5
PERMISSION = 6
COMMENTARY = 7
USER = 8

class RegisterPacker < BinData::Record
 	endian :big
	uint8 :type
	uint16 :len
	uint8 :status
end

class LoginPacket < BinData::Record
	endian :big
	uint8 :type
	uint16 :len
	uint8 :status
	uint32 :user_id, :onlyif => :succeeded?

	def succeeded?
		return status.zero?
	end
end

class Client
	def register
		@name = (0...15).map { ('a'..'z').to_a[rand(26)] }.join
		@password = (0...15).map { ('a'..'z').to_a[rand(26)] }.join
		send ClientPacket.new(ClientPacket::REGISTER).add(@name).add(@password)
	end

	def login
		send ClientPacket.new(ClientPacket::LOGIN).add(@name).add(@password)
	end

	def make_table
		
	end

	def make_task
	end

	def make_comment
	end

	def change_table
	end

	def change_task
	end

	def change_permission
	end

	def find_user
	end

	def do_register packet
		puts "Register packet"
		register_packet = RegisterPacker.read packet
	end

	def do_login packet
		puts "Login packet"
		login_packet = LoginPacket.read packet
		if login_packet.succeeded? then @id = login_packet.user_id end
	end
end

MAX_PACKET_SIZE = 60000

class User < Client
	def initialize params
		@socket = TCPSocket.open params.host, params.port
		@request = nil
		@responce = nil
		listen
		run params.timeout
		@responce.join
		@request.join
	end

	def listen
		@request = Thread.new do
			loop do
				packet = @socket.recv(MAX_PACKET_SIZE)
				parse packet
			end
		end
	end

	def run timeout
		@responce = Thread.new do
			loop do
				make_something
				sleep timeout
			end
		end
	end

	def send packet
		@socket.puts packet.pack
	end

	def parse packet
		type = packet.bytes.to_a[0][0]
		case type
		when REGISTER
			do_register packet
		when LOGIN
			do_login packet
		end
	end

	def make_something
		if @id.nil? then
			auth
		else
			[method(:make_table), method(:make_task), method(:make_comment), 
				method(:change_table), method(:change_task), method(:change_permission), 
					method(:find_user)].sample.call
		end
	end

	def auth
		@name.nil? ? register : login
	end
end

Params = Struct.new(:host, :port, :threads_count, :timeout) do
end

begin
	host = ARGV[0]
	port = ARGV[1].to_i
	threads_count = ARGV[2].to_i
	timeout = ARGV[3].to_i

	params = Params.new host, port, threads_count, timeout

	users_threads = []
	params.threads_count.times do
		users_threads << Thread.new() {
			User.new params
		}
		Signal.trap("CLD") do Thread.kill user_threads.last end
	end

	users_threads.each do |user|
		user.join
	end
end