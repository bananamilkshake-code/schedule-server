DROP DATABASE IF EXISTS schedule;
CREATE DATABASE schedule;

CREATE TABLE schedule.users (
	id INT(10) AUTO_INCREMENT PRIMARY KEY,
	login VARCHAR(40) NOT NULL,
	password VARCHAR(100) NOT NULL,

	name VARCHAR(50) NOT NULL,

	logout_time INT(10) NOT NULL,

	UNIQUE(login) 
); 

CREATE TABLE schedule.tables (
	id INT(10) AUTO_INCREMENT PRIMARY KEY
);

CREATE TABLE schedule.tasks (
	id INT(10) AUTO_INCREMENT PRIMARY KEY,
	table_id INT(10) NOT NULL,

	FOREIGN KEY (table_id) REFERENCES tables(id)
);

CREATE TABLE schedule.table_changes (
	table_id INT(10) NOT NULL,
	time INT(10) NOT NULL,
	user_id INT(10) NOT NULL,

	name VARCHAR(100),
	description TEXT,

	FOREIGN KEY (table_id) REFERENCES tables(id),
	FOREIGN KEY (user_id) REFERENCES users(id)
);

CREATE TABLE schedule.task_changes (
	task_id INT(10) NOT NULL,
	table_id INT(10) NOT NULL,
	time INT(10) NOT NULL,
	user_id INT(10) NOT NULL,

	name VARCHAR(100),
	description TEXT,
	completion_date DATE, 
	start_time TIME,
	end_time TIME,

	FOREIGN KEY (table_id) REFERENCES tables(id),
	FOREIGN KEY (task_id) REFERENCES tasks(id)
);

CREATE TABLE schedule.comments (
	commentator_id INT(10) NOT NULL,
	table_id INT(10) NOT NULL,
	task_id INT(10) NOT NULL,
	commentary TEXT NOT NULL,
	time INT(10) NOT NULL,

	FOREIGN KEY (commentator_id) REFERENCES users(id),
	FOREIGN KEY (table_id) REFERENCES tables(id),
	FOREIGN KEY (task_id) REFERENCES tasks(id)
);

CREATE TABLE schedule.readers (
	reader_id INT(10) NOT NULL,
	table_id INT(10) NOT NULL,
	permission TINYINT(1) NOT NULL,

	FOREIGN KEY (reader_id) REFERENCES users(id),
	FOREIGN KEY (table_id) REFERENCES tables(id)	
);