run_length=200
for scheduler in test_fifo_scheduler
do
    for num_nodes in 2 5 10
    do
        for size_d in 2 5 10
        do
            echo "[$scheduler] Setting num_nodes to $num_nodes, run_length to $run_length and size_d to $size_d."
            PERSIST=true NUM_RUNS=3 NUM_PROCESSES=$num_nodes RUN_LENGTH=$run_length SIZE_D=$size_d rebar3 ct --sname worker_$scheduler\_$num_nodes\_$size_d --suite=test/ra-kv-store_module_SUITE.erl --case=$scheduler &
        done
   done
done