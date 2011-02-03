create table users (
       id          SERIAL PRIMARY KEY,
       login       VARCHAR(127) UNIQUE NOT NULL, /* Need a case insensitive unique constraint! */
       password    VARCHAR(64) NOT NULL,
       email       VARCHAR(255)
);

create table repositories (
       id               SERIAL PRIMARY KEY,
       user_id          INTEGER NOT NULL REFERENCES users (id),
       git_ident        VARCHAR(255) NOT NULL,
       ssh_key          VARCHAR(2048) NOT NULL
);

create table nodes (
       id               SERIAL PRIMARY KEY,
       user_id          INTEGER NOT NULL REFERENCES users (id),
       repository_id    INTEGER NOT NULL REFERENCES repositories (id),
       aws_public_key   VARCHAR(20) NOT NULL,
       aws_secret_key   VARCHAR(40) NOT NULL
);

/* TODO: Session table */

create table sessions (
       id             SERIAL PRIMARY KEY,
       session_id     VARCHAR(64) UNIQUE NOT NULL,
       user_id        INTEGER NOT NULL REFERENCES users (id),
       expiration     DATE,
       client_address VARCHAR(64) /* ipv4, ipv6, bit of padding for other stuff. */
);

create user spirit;
grant connect on database torrent to spirit;
grant select, insert, update, delete on table users, repositories, nodes, sessions to spirit;
grant usage, select, update on sequence users_id_seq, repositories_id_seq, nodes_id_seq, sessions_id_seq to spirit;