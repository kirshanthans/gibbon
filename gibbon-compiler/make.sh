#!/bin/sh

debug="-D_GIBBON_GCSTATS -D_GIBBON_VERBOSITY=3 -D_GIBBON_DEBUG -DNURSERY_SIZE=512 -O0"
# debug=" -D_GIBBON_GCSTATS -O0"

gcc -std=gnu11  -fcilkplus -Wno-unused-variable -Wno-unused-label -Wall -Wextra -Wpedantic  -O3 $debug  -flto -I $GIBBONDIR/gibbon-compiler/cbits  -L$GIBBONDIR/gibbon-rts/target/debug -Wl,-rpath=$GIBBONDIR/gibbon-rts/target/debug -c $GIBBONDIR/gibbon-compiler/cbits/gibbon_rts.c -o $GIBBONDIR/gibbon-compiler/cbits/gibbon_rts.o  -lm -lgibbon_rts && gcc -std=gnu11  -fcilkplus  -Wno-unused-variable -Wno-unused-label -Wall -Wextra -Wpedantic  -O3 -g $debug  -flto     $GIBBONDIR/gibbon-compiler/cbits/gibbon_rts.o -I$GIBBONDIR/gibbon-compiler/cbits  -L$GIBBONDIR/gibbon-rts/target/debug    -Wl,-rpath=$GIBBONDIR/gibbon-rts/target/debug $GIBBONDIR/gibbon-compiler/examples/bench_new_rts.c    -o $GIBBONDIR/gibbon-compiler/examples/bench_new_rts.exe -lm -lgibbon_rts

gcc -std=gnu11  -fcilkplus  -Wno-unused-variable -Wno-unused-label -Wall -Wextra -Wpedantic  -O3 $debug  -flto -I $GIBBONDIR/gibbon-compiler/cbits  -L$GIBBONDIR/gibbon-rts/target/debug -Wl,-rpath=$GIBBONDIR/gibbon-rts/target/debug -c $GIBBONDIR/gibbon-compiler/cbits/gibbon_rts.c -o $GIBBONDIR/gibbon-compiler/cbits/gibbon_rts.o  -lm -lgibbon_rts && gcc -std=gnu11  -fcilkplus  -Wno-unused-variable -Wno-unused-label -Wall -Wextra -Wpedantic  -O3 -g  -flto  $debug   $GIBBONDIR/gibbon-compiler/cbits/gibbon_rts.o -I$GIBBONDIR/gibbon-compiler/cbits  -L$GIBBONDIR/gibbon-rts/target/debug    -Wl,-rpath=$GIBBONDIR/gibbon-rts/target/debug $GIBBONDIR/gibbon-compiler/examples/test_tree_update.c    -o $GIBBONDIR/gibbon-compiler/examples/test_tree_update.exe -lm -lgibbon_rts