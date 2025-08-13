#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to build project
build_project() {
    local build_dir=$1
    local toolchain=$2
    local platform=$3
    local extra_flags=$4
    
    echo -e "${YELLOW}========================================${NC}"
    echo -e "${GREEN}Building ${platform} version...${NC}"
    mkdir -p "${build_dir}"
    cd "${build_dir}" || exit 1
    
    local cmake_cmd="cmake .. -DCMAKE_BUILD_TYPE=Release"
    
    if [ -n "${toolchain}" ]; then
        cmake_cmd+=" -DCMAKE_TOOLCHAIN_FILE=${toolchain}"
    fi
    
    if [ -n "${extra_flags}" ]; then
        cmake_cmd+=" ${extra_flags}"
    fi
    
    echo -e "${YELLOW}Executing: ${cmake_cmd}${NC}"
    eval "${cmake_cmd}"
    
    cmake --build . --config Release
    cd ..
    echo -e "${GREEN}${platform} build completed in ${build_dir}${NC}"
    echo -e "${YELLOW}========================================${NC}"
    echo
}

# Clean previous builds
clean_builds() {
    echo -e "${YELLOW}Cleaning previous builds...${NC}"
    rm -rf build64 build32 build_linux build_linux32
    echo -e "${GREEN}Clean completed.${NC}"
    echo
}

# Install cross-compilation dependencies
install_dependencies() {
    echo -e "${YELLOW}Installing cross-compilation dependencies...${NC}"
    
    # Detect Linux distribution
    if [ -f /etc/debian_version ]; then
        echo "Debian/Ubuntu based system detected"
        sudo apt update
        sudo apt install -y gcc-multilib g++-multilib mingw-w64 cmake make
    elif [ -f /etc/arch-release ]; then
        echo "Arch Linux detected"
        sudo pacman -Syu --noconfirm
        sudo pacman -S --noconfirm gcc-multilib mingw-w64 cmake make
    elif [ -f /etc/redhat-release ]; then
        echo "RHEL/CentOS/Fedora detected"
        sudo dnf install -y gcc gcc-c++ glibc-devel.i686 mingw64-gcc cmake make
    else
        echo -e "${RED}Unsupported Linux distribution${NC}"
        return 1
    fi
    
    echo -e "${GREEN}Dependencies installed successfully!${NC}"
    echo
}

# Main menu
show_menu() {
    echo -e "${YELLOW}========================================${NC}"
    echo -e "${GREEN} Build System Menu${NC}"
    echo -e "${YELLOW}========================================${NC}"
    echo -e "1. Windows 64-bit"
    echo -e "2. Windows 32-bit"
    echo -e "3. Linux 64-bit"
    echo -e "4. Linux 32-bit"
    echo -e "5. Build ALL targets"
    echo -e "6. Clean ALL builds"
    echo -e "7. Install dependencies"
    echo -e "0. Exit"
    echo -e "${YELLOW}========================================${NC}"
}

# Main program loop
while true; do
    show_menu
    read -p "Select option (0-7): " choice
    
    case $choice in
        1)
            build_project "build64" "mingw-w64-x86_64.cmake" "Windows 64-bit"
            ;;
        2)
            build_project "build32" "mingw-w32-x86_64.cmake" "Windows 32-bit"
            ;;
        3)
            build_project "build_linux" "" "Linux 64-bit"
            ;;
        4)
            linux32_flags='-DCMAKE_CXX_FLAGS="-m32 -L/usr/lib32" -DCMAKE_C_FLAGS="-m32 -L/usr/lib32"'
            build_project "build_linux32" "" "Linux 32-bit" "${linux32_flags}"
            ;;
        5)
            echo -e "${GREEN}Building ALL targets...${NC}"
            build_project "build64" "mingw-w64-x86_64.cmake" "Windows 64-bit"
            build_project "build32" "mingw-w32-x86_64.cmake" "Windows 32-bit"
            build_project "build_linux" "" "Linux 64-bit"
            linux32_flags='-DCMAKE_CXX_FLAGS="-m32 -L/usr/lib32" -DCMAKE_C_FLAGS="-m32 -L/usr/lib32"'
            build_project "build_linux32" "" "Linux 32-bit" "${linux32_flags}"
            echo -e "${GREEN}All builds completed!${NC}"
            ;;
        6)
            clean_builds
            ;;
        7)
            install_dependencies
            ;;
        0)
            echo -e "${GREEN}Exiting. Goodbye!${NC}"
            exit 0
            ;;
        *)
            echo -e "${RED}Invalid choice. Please try again.${NC}"
            ;;
    esac
    
    read -p "Press Enter to continue..."
done
