On Windows using MSYS2, I needed to add this to libpd/pure-data/src/m_pd.h:

#ifdef _WIN32
#include <malloc.h>
#endif

(near the top after 'extern "c"...' is fine)
I haven't found a better way around this yet.