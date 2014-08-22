NOTITLE=1
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# multiple zsh share history
setopt SHARE_HISTORY

# store time etc.
setopt EXTENDED_HISTORY

# store duplicate entries but do not search for them
setopt HIST_FIND_NO_DUPS

# remove whitespace
setopt HIST_REDUCE_BLANKS

# emacs key bindings
bindkey -e

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/marco/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

path=('/home/marco/.cabal/bin/' $path)
export PATH

# prompt
# PS1='%D{%L:%M}%# %~/ '

setopt AUTO_NAME_DIRS
qinqu=~/workspace/haskell/q-inqu
henge=~/workspace/haskell/henge
boom=~/workspace/haskell/henge/games/boom
hsys=~/workspace/haskell/henge/hsys
halo=~/workspace/haskell/henge/halo

setopt AUTO_CD
setopt CORRECT
setopt CORRECT_ALL

[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" history-beginning-search-forward
[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" history-beginning-search-backward

# dirstack
DIRSTACKFILE="$HOME/.cache/zsh/dirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
    dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
    [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi

chpwd() {
    print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}

DIRSTACKSIZE=20

setopt autopushd pushdsilent pushdtohome
setopt pushdignoredups
setopt pushdminus

source /etc/zsh/.zshrc
