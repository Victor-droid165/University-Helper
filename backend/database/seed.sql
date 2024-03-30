INSERT INTO users
VALUES (
        'everton',
        'everton@admin.ufcg.edu.br',
        'senhaSegura',
        'Admin',
        '1195010000',
        'UFCG'
    );
INSERT INTO labels
VALUES ('IMPORTANTE', 'ff0000'),
    ('ARQUIVADO', '00ffff'),
    ('EM CURSO', '0000ff'),
    ('TRANCADO', 'ff9900'),
    ('APROVADO', '00ff00'),
    ('REPROVADO', '980000');
INSERT INTO default_note_event_labels
VALUES (1),
    (2);
INSERT INTO default_class_labels
VALUES (3),
    (4),
    (5),
    (6);