print "Enter end to start. "
script = []
line = raw_input(">>> ")
while line != "end":
    script.append(line)
    line = raw_input("... ")

if script[0] != "end":
    script[0] = "\"" + script[0] + "\" ^"
    script = [script[0]] + \
            ["\"\\n" + line + "\" ^" for line in script[1:-1]] + \
            ["\"\\n" + script[-1] + "\""]
    for l in script:
        print l
