(comment
  "from wiki on S-expressions an SExpr parser example:"
  (defn parse_sexp [string]
    (let [
          sexp [[]]
          word ''
          in_str False]      
      (for [c string]
        (if (and (= c "(") and not in_str:)
                           (
                            sexp.append([])
                                       elif c == '' and not in_str:
                                       if(len(word) > 0):
                                       sexp[-1].append(word)
                                       word = ''
                                       temp = sexp.pop()
                                       sexp[-1].append(temp)
                                       elif c in (' ', '\n', '\t') and not in_str:
                                       sexp[-1].append(word)
                                       word = ''
                                       elif c == '\"':
                                       in_str = not in_str
                                       else:
                                       word = word + c
                                       return
                                       sexp[0])))))))
  )
