# anagram ['cinema', 'iceman', 'diog', 'god']
def anagram(input_list):
    tmp_list = list(input_list)
    for i in input_list:
        tmp_list.remove(i)
        for j in tmp_list:
            if len(i) == len(j):
                if sorted(i) == sorted(j):
                    print(i, 'and', j, 'are Anagram')
anagram(['cinema', 'iceman', 'diog', 'god'])

# palindrome 'eve'
def palindrome(input_string):
    if input_string == input_string[::-1]:
        print(input_string, 'is Palindrome')
    else:
        print(input_string, 'is not Palindrome')
palindrome('eve')

# star triangle 4
a = 5
for i in range(a):
    print(' '*(a-i-1)+'*'*(2*i+1))

# permutations 'abbcc'
def permutations(input_string):
    output_list = []
    if len(input_string) == 1:
        return input_string
    else:
        for i in input_string:
            [output_list.append(i + j) for j in permutations(input_string.replace(i, '', 1))]
        return output_list
# print(permutations('abbcc'))
print(list(dict.fromkeys(permutations('abbcc'))))

# string reverse 'this is a string'
a = 'this is a string'
b = a.split(' ')
b.reverse()
print(' '.join(b))

# longest non repeating substring in a string 'pwwkew'
def substring(input_string):
    output_dict = {}
    tmp_string = ''
    for i in range(len(input_string)):
        j = i + 1
        if j < len(input_string):
            if input_string[i] != input_string[j] and input_string[j] not in tmp_string:
                if len(tmp_string) == 0:
                    tmp_string = input_string[i] + input_string[j]
                else:
                    tmp_string += input_string[j]
                if len(tmp_string) in output_dict.keys():
                    output_dict[len(tmp_string)].append(tmp_string)
                else:
                    output_dict[len(tmp_string)] = [tmp_string]
            else:
                tmp_string = ''
    print(output_dict)
substring('pwwkew')

# - Given positive integers A, B, and N, write a program that prints integers from 1 to N. But for integers that are multiples of A, print 'F', and for multiples of B, print 'B'. For integers which are multiples of both A and B, print 'FB'.
# - flow through how many times https://raw.githubusercontent.com/ganasubrgit/tom-and-jerry/main/tandJ.txt
# - evaluvate dictonary 'https://raw.githubusercontent.com/malepati/adhocwork/master/ansible/list.json'
# "/Users/snehasetty/aaa/adhocwork/ansible/dict.json"
# - Parent,child class
