SET search_path TO "uh_schema";

-- Create the users table
CREATE TABLE users (
    name VARCHAR(60) NOT NULL,
    email VARCHAR(80) UNIQUE NOT NULL,
    password VARCHAR(50) NOT NULL,
    type VARCHAR(10) CHECK (type IN ('Admin', 'Student', 'Professor', 'Visitor')) NOT NULL,
    enrollment_number VARCHAR(20),
    university_name VARCHAR(100),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    id SERIAL PRIMARY KEY
);

CREATE TABLE admin_user_validations (
    admin_id INT NOT NULL,
    user_id INT UNIQUE NOT NULL,
    validation_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (admin_id, user_id),
    FOREIGN KEY (admin_id) REFERENCES users(id),
    FOREIGN KEY (user_id) REFERENCES users(id)
);

CREATE TABLE notebooks (
    type VARCHAR(20) CHECK (type IN ('Convencional', 'Cronológico', 'Mental')) NOT NULL,
    name VARCHAR(100) UNIQUE NOT NULL,
    id SERIAL PRIMARY KEY
);

CREATE TABLE notebook_subjects (
    notebook_id INT NOT NULL,
    name VARCHAR(100) NOT NULL,
    PRIMARY KEY (notebook_id, name),
    FOREIGN KEY (notebook_id) REFERENCES notebooks(id)
);

CREATE TABLE pages (
    notebook_id INT NOT NULL,
    subject_name VARCHAR(100) NOT NULL,
    page_number INT NOT NULL,
    id SERIAL PRIMARY KEY,
    FOREIGN KEY (notebook_id, subject_name) REFERENCES notebook_subjects(notebook_id, name)
);

-- Create the notes table
CREATE TABLE notes (
    id VARCHAR(50) PRIMARY KEY,
    type VARCHAR(20) CHECK (type IN ('Reminder', 'StickyNote', 'PlainText', 'Warning')) NOT NULL,
    visibility VARCHAR(10) DEFAULT 'Public' CHECK (visibility IN ('Private', 'Public')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_edited_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    title VARCHAR(255),
    subject VARCHAR(255),
    content TEXT NOT NULL,
    creator_id INT NOT NULL,
    page_id INT,
    FOREIGN KEY (creator_id) REFERENCES users(id),
    FOREIGN KEY (page_id) REFERENCES pages(id)
);

CREATE TABLE note_contestations (
    note_id VARCHAR(50) NOT NULL,
    user_id INT NOT NULL,
    contestation_comment TEXT NOT NULL,
    resolution_comment TEXT,
    resolved_by INT,
    contestation_created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (note_id, user_id),
    FOREIGN KEY (note_id) REFERENCES notes(id),
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (resolved_by) REFERENCES users(id)
);