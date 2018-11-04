for f in tests/*; 
do 
    out="`./opython $f`"
    if ((${#out} \> 0))
    then
        echo "'$out' in $f"
    fi
done 