#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/callback.h>

#include "decompress.h"

int
main(int argc, const char **argv)
{
  char *inbuf;
  char *outbuf;
  FILE *fh = (strcmp(argv[1], "-i") == 0) ? fopen(argv[3], "rb") : fopen(argv[1], "rb");

  if (fh != NULL)
    {
      fseek(fh, 0L, SEEK_END);
      long insize = ftell(fh);
      long outsize = (strcmp(argv[1], "-i") == 0) ? atoi(argv[2]) : insize;
      rewind(fh);

      inbuf = malloc(insize);
      outbuf = malloc(outsize);

      if (inbuf != NULL && outbuf != NULL)
        {
          (void) fread(inbuf, insize, 1, fh);
          fclose(fh);
          fh = NULL;

          long outlen = 0;

          printf("Start to call OCaml function.\n");

          if (strcmp(argv[1], "-i") == 0)
            outlen = inflate(inbuf, insize, outbuf, outsize, 4, 0, NULL);
          else
            outlen = deflate(inbuf, insize, outbuf, outsize, 4, 0, NULL);

          for (int i = 0; i < outlen; ++i, ++outbuf)
            printf("%c", *outbuf);

          free(inbuf);
          free(outbuf);
          inbuf = NULL;
          outbuf = NULL;
        }

      if (inbuf != NULL)  free(inbuf);
      if (outbuf != NULL) free(outbuf);
      if (fh != NULL)     fclose(fh);
    }

  return EXIT_SUCCESS;
}
