dist/build/codegen/codegen test.c > test.ll
clang -S -emit-llvm main.c -c -o main.ll
llvm-link main.ll test.ll -o main.bc
llc main.bc -o main.s 
cc main.s -o main  
chmod +x main