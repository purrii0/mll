#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <unordered_map>

struct Instruction {
    std::string opcode;
    std::string operands;
};

class stack{
private:
	int top;
	int arr[256];
public:
	stack() { top = -1; }

	void push(int x) {
		if (top >= 255)
		{
            std::cout << "Stack overflow" << std::endl;
            return;
		}
		arr[++top] = x;
	}
	int pop() {
		if (top < 0)
		{
			std::cout << "Stack underflow" << std::endl;
            return 0;
		}
		return arr[top--];		
	}
	int peek(){
		if (top < 0) {
            std::cout << "Stack is empty" << std::endl;
            return 0;
        }
		return arr[top];
	}
	bool isEmpty() {
		return (top < 0);
	}
};


bool hasExtension(const std::string& filename) {
    const std::string extension = ".mll";
    if (filename.size() >= extension.size()) {
        return filename.compare(filename.size() - extension.size(), extension.size(), extension) == 0;
    }
    return false;
}

std::pair<std::string, std::string> parseLine(const std::string& line) {
    size_t pos = line.find(' ');
    if (pos == std::string::npos) {
        return { line, "" };
    }
    std::string opcode = line.substr(0, pos);
    std::string operand = line.substr(pos + 1);
    return { opcode, operand };
}


int main(int argc, char *argv[])
{
	if (argc < 2) {
        std::cerr << "No input file provided." << std::endl;
        return 1;
    }
	if (!hasExtension(argv[1]))
	{
		std::cerr << "Invalid file provided. Please supply a valid .mll file." << std::endl;
		return 1;
	}
	std::ifstream file(argv[1]);
	if (!file.is_open())
	{
		std::cerr << "Failed to Open File" << std::endl;
		return 1;
	}
	stack s;
	std::string token;
	std::vector<Instruction> programs;
	std::unordered_map<std::string, int> labelMap;
	int instructionIndex = 0;
	while (std::getline(file, token))
	{
		auto [opcode, operand] = parseLine(token);
		if (!opcode.empty() && opcode.back() == ':') {
		    labelMap[opcode.substr(0, opcode.size() - 1)] = instructionIndex;
		    continue;
		}
		programs.push_back({
			opcode,
			operand
		});
		instructionIndex++;
	}

	int pc = 0;
	while (pc < programs.size())
	{
		Instruction& inst = programs[pc];
		if (inst.opcode == "READ")
		{
			int x;
			std::cin >> x;
			s.push(x);	
		}
		if (inst.opcode == "PUSH")
		{
			s.push(std::stoi(inst.operands));
		}
		if (inst.opcode == "SUB")
		{
			int x = s.pop();
			int y = s.pop();
			int a = y - x;
			s.push(a);
		}
		if (inst.opcode == "ADD")
		{
			int x = s.pop();
			int y = s.pop();
			int a = y + x;
			s.push(a);
		}
		if (inst.opcode == "JUMP.EQ.0")
		{
			int x = s.peek();
			if(x == 0) {
				pc = labelMap[inst.operands];
				continue; 
			}
			s.push(x);
		}
		if (inst.opcode == "JUMP.GT.0")
		{
			int x = s.peek();
			if(x > 0) {
				pc = labelMap[inst.operands];
				continue;
			}
		}
		if (inst.opcode == "PRINT")
		{
			std::string out = inst.operands;
			if (!out.empty() && out.front() == '"' && out.back() == '"') {
			    out = out.substr(1, out.size() - 2);
			}
			std::cout << out << std::endl;			
		}
		if (inst.opcode == "POP")
		{
			std::cout << s.pop() << std::endl;
		}
		
		if (inst.opcode == "HALT")
		{
			return 0;
		}	
		pc++;
	}
	return 0;
}


// might do
/* 
Language features
	Add MUL instruction for multiplication
	Add DIV instruction for integer division
	Add MOD instruction for modulo
	Add DUP to duplicate top of stack
	Add SWAP to swap top two elements
	Add OVER to copy the second element to top

Control flow
	Add JUMP.LT.0, JUMP.GE.0, JUMP.NE.0 for richer conditions
	Support CALL and RET for subroutine-like jumps (call stack)

Program & language
	Allow comments with ; or # to ignore rest of line
	Better PRINT that handles numbers from stack directly (no operand needed)
	Maybe support PRINTLN vs PRINT for newline control

Memory / variables
	Add STORE x and LOAD x to work with named slots (like variables)
	Maybe add CLEAR x to reset a variable

Interpreter engine
	Improve error messages: print instruction + stack on error
	Implement a DEBUG mode that prints pc, current instruction, and stack
	Catch invalid jumps or unknown opcodes gracefully

Optional interactive features
 	Make an interactive REPL mode (enter instructions live)
 	Support input from user for variable names

Example advanced features (future)
	File reading / writing instructions (READFILE, WRITEFILE)
 	Arrays or list-like stack operations (PUSHALL, POPALL)
*/