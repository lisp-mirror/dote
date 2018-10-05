BEGIN {
    TRUE        = 1;
    FALSE       = 0;
    VERSION_SEP = "\\.";
}

function split_version_number (version, parsed) {
    split(version, parsed, VERSION_SEP);
    for (i in parsed) {
        parsed[i] = strtonum(parsed[i]);
    }
}

function version_less_than_p (version_a, version_b,     idx) {
    for (idx=1; idx <= length(version_a); idx++){
        if(version_a[idx] < version_b[idx]){
            return TRUE;
        } else if(version_a[idx] > version_b[idx]){
            return FALSE;
        }
    }
    return FALSE;
}


// {
    versions[""]="";
    version_a[""]="";
    version_b[""]="";
    split($0, versions, "[[:space:]]+");
    split_version_number(versions[1], version_a);
    split_version_number(versions[2], version_b);
    print (version_less_than_p(version_a, version_b));

}
