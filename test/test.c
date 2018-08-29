#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/callback.h>

#include "dcpr.h"

int max(int a, int b) { return (a < b) ? b : a; }

int
main(int argc, const char **argv)
{
  char *inbuf;
  char *outbuf;
  FILE *fh = (strcmp(argv[1], "-i") == 0) ? fopen(argv[2], "rb") : fopen(argv[1], "rb");

  if (fh != NULL)
    {
      fseek(fh, 0L, SEEK_END);
      long insize = ftell(fh);
      long outsize = insize * 2;
      rewind(fh);

      inbuf = malloc(insize);
      outbuf = malloc(outsize);

      if (inbuf != NULL && outbuf != NULL)
        {
          long inlen = fread(inbuf, sizeof(char), insize, fh);
          fclose(fh);
          fh = NULL;

          long outlen = 0;

          if (strcmp(argv[1], "-i") == 0)
            outlen = dcpr_inflate(inbuf, insize, outbuf, outsize, 4, 0, NULL);
          else
            outlen = dcpr_deflate(inbuf, insize, outbuf, outsize, 4, 0, NULL);

          for (int i = 0; i < outlen; ++i)
            printf("%c", outbuf[i]);

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
