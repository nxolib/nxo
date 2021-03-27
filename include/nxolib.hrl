-define (IS_STRING(Term),
    (is_list(Term) andalso Term /= [] andalso is_integer(hd(Term)))).
