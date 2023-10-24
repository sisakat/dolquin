#define STB_IMAGE_IMPLEMENTATION
#include <stdio.h>
#include "stb_image.h"

uint8_t* load_image(const char* filename, int* width, int* height)
{
    int comp;
    uint8_t* result = stbi_load(filename, width, height, &comp, 3);
    if (comp != 3) {
        return NULL;
    }
    return result;
}
