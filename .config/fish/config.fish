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
    if test -d /nix
       set PATH $PATH /nix/var/nix/profiles/default/bin
    end
    if test -d $HOME/Library/Python/3.10/bin
       set PATH $PATH $HOME/Library/Python/3.10/bin
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
