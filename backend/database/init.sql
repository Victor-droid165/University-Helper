SET search_path TO "uh_schema";

-- In our app, users must have name, email, password and a type.
-- users can also take part on a university, thus, having an enrollment number and
-- a field with the name of the university they attend.
CREATE TABLE users (
    name VARCHAR(60) NOT NULL,
    email VARCHAR(80) UNIQUE NOT NULL,
    password VARCHAR(50) NOT NULL,
    type VARCHAR(10) CHECK (type IN ('Admin', 'Student', 'Professor')) NOT NULL,
    enrollment_number VARCHAR(20),
    university_name VARCHAR(100),
    is_deleted BOOLEAN DEFAULT FALSE, -- Enables soft deletion
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    id SERIAL PRIMARY KEY
);

-- Table to model the relationship: user of type ADMIN VALIDATES user of type non-admin
CREATE TABLE admin_user_validations (
    admin_id INT NOT NULL,
    user_id INT UNIQUE NOT NULL,
    validation_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (admin_id, user_id),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- In our app, Notes have an id, a type, a visibility, and a content.
-- Also, it must have been created by a user and may have title and/or subject.
CREATE TABLE notes (
    id VARCHAR(24) PRIMARY KEY,
    type VARCHAR(10) CHECK (type IN ('Reminder', 'StickyNote', 'PlainText', 'Warning')) NOT NULL,
    visibility VARCHAR(10) DEFAULT 'Private' CHECK (visibility IN ('Private', 'Public')),
    title VARCHAR(255),
    subject VARCHAR(255),
    content TEXT NOT NULL,
    creator_id INT NOT NULL,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (creator_id) REFERENCES users(id) ON DELETE CASCADE
);

-- In our app, users can contest the note deletion during an interval of 3 days after the action.
-- For this we created the following:
CREATE TABLE notes_to_delete (
    note_id VARCHAR(24) NOT NULL,
    deleted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (note_id) REFERENCES notes(id) ON DELETE CASCADE
);

-- In our app, PROFESSOR AND/OR ADMIN WARNS USERS, so the table below is for that.
CREATE TABLE user_warnings (
    warning_id VARCHAR(24) NOT NULL,
    warned_user_id INT NOT NULL,
    PRIMARY KEY (warning_id, warned_user_id),
    FOREIGN KEY (warning_id) REFERENCES notes(id) ON DELETE CASCADE,
    FOREIGN KEY (warned_user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- In our app, notebooks can be of three types, and all of 'em must have a name.
CREATE TABLE notebooks (
    type VARCHAR(15) CHECK (type IN ('Convencional', 'Cronológico', 'Mental')) NOT NULL,
    name VARCHAR(100) UNIQUE NOT NULL,
    id SERIAL PRIMARY KEY
);

-- In our app, 'Convencional' notebooks are divided into subjects. Plus,
-- notes can also be labeled with many subjects. For this, we created the table below.
CREATE TABLE subjects (
    name VARCHAR(100) UNIQUE NOT NULL,
    id SERIAL PRIMARY KEY
);

-- In our app, 'Convencional' and 'Chronological' (Normal) notebooks can have pages.
-- These pages have a number and are part of a notebook.
CREATE TABLE pages (
    normal_notebook_id INT NOT NULL,
    page_number INT NOT NULL,
    id SERIAL PRIMARY KEY,
    FOREIGN KEY (normal_notebook_id) REFERENCES notebooks(id) ON DELETE CASCADE
);

-- Table to represent mental notebook nodes
CREATE TABLE mental_notebook_nodes (
    label VARCHAR(50) NOT NULL,
    id SERIAL PRIMARY KEY
);

-- Table to represent the relationship between mental notebook nodes and regular notes
CREATE TABLE mental_notebook_node_notes (
    mental_notebook_node_id INT NOT NULL,
    note_id VARCHAR(24) NOT NULL,
    PRIMARY KEY (mental_notebook_node_id, note_id),
    FOREIGN KEY (mental_notebook_node_id) REFERENCES mental_notebook_nodes(id),
    FOREIGN KEY (note_id) REFERENCES notes(id)
);

-- Table to represent the relationship between mental notebook nodes and entire notebooks
CREATE TABLE mental_notebook_node_notebooks (
    mental_notebook_node_id INT NOT NULL,
    notebook_id INT NOT NULL,
    PRIMARY KEY (mental_notebook_node_id, notebook_id),
    FOREIGN KEY (mental_notebook_node_id) REFERENCES mental_notebook_nodes(id),
    FOREIGN KEY (notebook_id) REFERENCES notebooks(id)
);

-- Table to represent connections between mental notebook nodes
CREATE TABLE mental_notebook_node_connections (
    source_node_id INT NOT NULL,
    target_node_id INT NOT NULL,
    label VARCHAR(50) NOT NULL,
    PRIMARY KEY (source_node_id, target_node_id),
    FOREIGN KEY (source_node_id) REFERENCES mental_notebook_nodes(id),
    FOREIGN KEY (target_node_id) REFERENCES mental_notebook_nodes(id)
);

-- A note can be labeled with many subjects. A subject can label many notes.
-- For this, we created the table below.
CREATE TABLE note_subject (
    note_id VARCHAR(24) NOT NULL,
    subject_id INT NOT NULL,
    PRIMARY KEY (note_id, subject_id),
    FOREIGN KEY (note_id) REFERENCES notes(id) ON DELETE CASCADE,
    FOREIGN KEY (subject_id) REFERENCES subjects(id) ON DELETE CASCADE
);

-- A note may be either written or copied into a page. Either way, it will
-- take part in one or more pages either completely or partially. For this,
-- we created the following:
CREATE TABLE note_page (
    page_id INT NOT NULL,
    note_id VARCHAR(24) NOT NULL,
    start_position INT NOT NULL,
    end_position INT,
    PRIMARY KEY (page_id, note_id),
    FOREIGN KEY (page_id) REFERENCES pages(id) ON DELETE CASCADE,
    FOREIGN KEY (note_id) REFERENCES notes(id) ON DELETE CASCADE
);

-- A note can be contested by an user. If so, the user will send a comment
-- and this will be resolved by an admin.
CREATE TABLE note_contestations (
    note_id VARCHAR(24) NOT NULL,
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