(in-package :cl-user)
(defpackage :lispmud/tables
  (:use cl))
(in-package :lispmud/tables)

(defun create-players ()
  (pomo:execute "create table players (
                     name text not null,
                     password text not null,
                     online_p boolean not null,
                     money_in_bank integer not null default 0,
                     primary key (name) );"))

(defun create-letters ()
  (pomo:execute "create table letters (
                     id serial primary key,
                     sender_name text references players(name) on delete cascade,
                     receiver_name text references players(name) on delete cascade,
                     message_text text not null );"))

(defun total-drop ()
  (pomo:execute "drop table if exists letters")
  (pomo:execute "drop table if exists players"))

(defun recreate-all-tables (db-name db-user db-password db-host)
  (pomo:with-connection (list db-name db-user db-password db-host)
    (total-drop)
    (create-players)
    (create-letters)))
