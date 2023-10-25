import LexicalFunctions

while True:
    text = input('Elfin : INPUT--> ')
    result, error = LexicalFunctions.run('<stdin>', text)

    if error:
        print(error.stringTypeError())
    else:
        print(result)