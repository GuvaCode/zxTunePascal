# Static Windows 64-bit toolchain with MinGW-w64
set(CMAKE_SYSTEM_NAME Windows)
set(TOOLCHAIN_PREFIX x86_64-w64-mingw32)

# Компиляторы
set(CMAKE_C_COMPILER ${TOOLCHAIN_PREFIX}-gcc)
set(CMAKE_CXX_COMPILER ${TOOLCHAIN_PREFIX}-g++)
set(CMAKE_RC_COMPILER ${TOOLCHAIN_PREFIX}-windres)

# Пути поиска
set(CMAKE_FIND_ROOT_PATH /usr/${TOOLCHAIN_PREFIX})

# Настройки FIND
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

# Критически важные настройки для статической линковки
set(CMAKE_EXE_LINKER_FLAGS_INIT "-static -static-libgcc -static-libstdc++ -Wl,--subsystem,windows")
set(CMAKE_SHARED_LINKER_FLAGS_INIT "-static -static-libgcc -static-libstdc++ -Wl,--subsystem,windows")
set(CMAKE_MODULE_LINKER_FLAGS_INIT "-static -static-libgcc -static-libstdc++ -Wl,--subsystem,windows")

# Дополнительные флаги компиляции
set(CMAKE_CXX_FLAGS "-mwin32 -mthreads")
set(CMAKE_C_FLAGS "-mwin32 -mthreads")

# Отключаем тестовую компиляцию GUI-приложения
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
