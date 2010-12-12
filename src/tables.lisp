(in-package :lispmud)

(defun create-players ()
  (pomo:execute "create table players (
                     name text not null,
                     password text not null,
                     online_p boolean not null,
                     money_in_bank integer not null,
                     primary key (name) );"))

(defun create-letters ()
  (pomo:execute "create table letters (
                     id serial primary key,
                     sender_name text references players(name) on delete cascade,
                     receiver_name text references players(name) on delete cascade,
                     message_text text not null );"))

(defun total-drop ()
  (pomo:execute "drop table letters")
  (pomo:execute "drop table players"))