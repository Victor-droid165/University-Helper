-- Insert initial data
INSERT INTO users (username, email, password_hash) VALUES
    ('john_doe', 'john@example.com', 'hashed_password_1'),
    ('jane_doe', 'jane@example.com', 'hashed_password_2');

INSERT INTO posts (title, content, user_id) VALUES
    ('First Post', 'This is the content of the first post.', 1),
    ('Second Post', 'This is the content of the second post.', 2);

INSERT INTO comments (content, post_id, user_id) VALUES
    ('Great post!', 1, 2),
    ('Nice article!', 2, 1);
