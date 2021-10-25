 # patch ekv file

t = []
with open ('ekv.orig', 'r') as f:
    for line in f.readlines():
        if ' `P(' in line:
            ix0 = len(line) - len(line.lstrip()) # find leading white space
            ix1 = line.find(' `P(') # start of the macro
            
            # find matching closing bracket
            lvl = 0
            ix2 = ix1
            while True:
                ix2 += 1
                if line[ix2] == ')':
                    lvl -= 1
                    if lvl == 0:
                        break
                elif line[ix2] == '(':
                    lvl += 1
                    
            attribs = line[ix1+4:ix2] # the part containing all the extra attibutes
            a = '(* ' + ', '.join(attribs.split()) + ' *) ' # add separating comma's 
            s = line[:ix0] + a  +  line[ix0:ix1] + line[ix2+1:] # reorder line
            t.append(s)
        else:
            t.append(line)
            
with open('ekv.va', 'w') as fo:
    fo.writelines(t) # write the final text