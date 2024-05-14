#define NOBUILD_IMPLEMENTATION
#include "./nobuild.h"

#define CFLAGS "-Wall", "-Wextra", "-std=c99", "-pedantic"


int main(int argc, char **argv)
{
    GO_REBUILD_URSELF(argc, argv);

    if (!path_exists("build")){
        MKDIRS("build");
    }
        CMD("cc", CFLAGS, "-o", "build/cortex", "src/main.c");
        CMD("./build/cortex");

    return 0;
}
