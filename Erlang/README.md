Here are some programs in Erlang:

1) db_list.erl:

A simple database server, using type list().

2) db_record.erl (+ db_record.hrl):

The database server using records.

3) db_ets.erl (db_dets.erl):

The database server using ETS. In the db_ets.erl - the database is stored in the RAM, when in the db_dets.erl - the database is stored on the disk.

4) db_distr.erl:

The distributed database server using ETS.

5) master_slave.erl:

There is a process (the master) which supervises other processes (the slaves). The master's job is to ensure that all the slave processes are alive. If a slave crashes, the master is to recreate the failed slave. 

6) pingpong.erl:

A message is sent forwards and backwards between 2 processed M times.

7) ring.erl:

N processes are started in a ring. A message is sent M times around all the processes in the ring.

8) star.erl:

N processes are started in a star. A message is sent M times between processes in the star through the central process.
