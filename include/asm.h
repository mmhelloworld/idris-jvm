#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void createJvm(void** jvm, void** env, int optionsLen, char** optionStr);
extern void assemble(JNIEnv *env, int argc, char **argv);
extern void destroyJvm(JavaVM *jvm);

#ifdef __cplusplus
}
#endif
