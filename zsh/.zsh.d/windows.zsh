if [[ -n "$WSL_DISTRO_NAME" ]]; then
    function xrun {
        export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0
        export LIBGL_ALWAYS_INDIRECT=1
        setsid $1
        exit
    }

    alias clip=clip.exe
fi
