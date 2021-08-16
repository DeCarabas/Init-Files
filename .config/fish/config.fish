if status --is-login
    set PATH /opt/local/bin /opt/local/sbin $PATH ~/bin
    if test -d ~/.cargo/bin
       set PATH ~/.cargo/bin $PATH
    end
    if test -d ~/devtools/buck/bin
       set PATH $PATH ~/devtools/buck/bin
    end
    if test -d /snap/bin
       set PATH $PATH /snap/bin
    end
    if test -d ~/.local/bin
       set PATH $PATH ~/.local/bin
    end
end

if [ -n "$INSIDE_EMACS" ]
  # This is here to make emacs and ansi-term work properly; I'm not *quite*
  # sure what it does but it's probably cool.
  function fish_title
    true
  end

 # emacs dir tracking
 function prompt_AnSiT -e fish_prompt
    printf "\eAnSiTc %s\n" "$PWD"
  end
  printf "\eAnSiTu %s\n" "$USER"
end

# export EDITOR=ec
# export VISUAL=ec

# export FBANDROID_DIR=/Users/doty/fbsource/fbandroid
# alias quicklog_update=/Users/doty/fbsource/fbandroid/scripts/quicklog/quicklog_update.sh
# alias qlu=quicklog_update

# # added by setup_fb4a.sh
# export ANDROID_SDK=/opt/android_sdk
# export ANDROID_NDK_REPOSITORY=/opt/android_ndk
# export ANDROID_HOME={ANDROID_SDK}
# export PATH={PATH}:{ANDROID_SDK}/tools:{ANDROID_SDK}/platform-tools
