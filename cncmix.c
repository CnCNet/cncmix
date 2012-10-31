#include <stdio.h>
#include <stdint.h>

#define ROL(n) ((n<<1) | ((n>>31) & 1))

uint32_t calc_id (char fname[])
{
    uint32_t id;
    union {char chars[13]; long longs[3];} buffer = {};
    int i = 0;
    
    for (i=0; fname[i]!='\0' && i<12; i++)
    {
	buffer.chars[i] = toupper(fname[i]);
    }
    
    for (i=0; buffer.longs[i]!=0 && i<4; i++)
    { 
	id = ROL(id) + buffer.longs[i];
    }
    return id;
}

int main(int argc, char* argv[] )
{
    if (argc != 2)
    {
	fprintf(stderr,"derp, 1 arg plz\n");
	return (-1);
    }
    printf( "ID = %lX\n", calc_id(argv[1]));
    return 1;
}
