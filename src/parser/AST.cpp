#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include <memory>
#include <string>
#include <vector>
#include <map>
#include <cctype>
#include <cstdio>
#include <cstdlib>

using namespace llvm;

/*
* To build this program, the command to do it via clang++ is:
* 
* /Users/raymondtay/llvm/build/./bin/clang++ \
* -std=c++14 \
* -I/Users/raymondtay/llvm/include \
* -I/Users/raymondtay/llvm/build/include -g -O3 ./AST.cpp \
* `/Users/raymondtay/llvm/build/bin/llvm-config --cxxflags --ldflags --system-libs --libs core` \
* -D__STDC_LIMIT_MACROS -o ./lexer
* 
* Note:
* (a) llvm is checked out to /Users/raymondtay/llvm directory
* (b) llvm is build to /Users/raymondtay/llvm/build
* (c) There are two include directories here which houses different header files clang++ is looking for
*/

namespace helper {

template<typename T, typename... Args>
  static typename std::enable_if<!std::is_array<T>::value, std::unique_ptr<T>>::type
  make_unique(Args&& ... args) {
    return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
  }

}
//===----------------------------------------------------------------------===//
//// Lexer
////===----------------------------------------------------------------------===//
//
//// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
//// of these for known things.
enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

static int gettok() {
  static int LastChar = ' ';
  while (isspace(LastChar))
    LastChar = getchar();

  if (isalpha(LastChar)) {
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())))
      IdentifierStr += LastChar;

    if (IdentifierStr == "def")
      return tok_def;
    if (IdentifierStr == "extern")
      return tok_extern;
    return tok_identifier;
  }
  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    return tok_number;
  }

  if (LastChar == '#') {
    // Comment until end of line.
    do
      LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
    
    if (LastChar != EOF)
      return gettok();
  }
     
  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;
     
  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

  /**
   * the codegen() method says to emit IR for that AST node along with all the things 
   * it depends on, and they all return an LLVM Value object. 
   * "Value" is the class used to represent a "Static Single Assignment" i.e.SSA
   */
static std::unique_ptr<Module> TheModule;
static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, Value*> NamedValues;


class ExprAST {
  public:
    virtual ~ExprAST() {}
    virtual Value *codegen() = 0;
};

std::unique_ptr<ExprAST> Error(const char* Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

Value* ErrorV(const char* str) {
  Error(str);
  return nullptr;
}

class NumberExprAST : public ExprAST {
private:
  double val;
public:
  NumberExprAST(double val) : val(val) {}
  virtual Value *codegen() ;
};

// 
// In the LLVM IR, numeric constants are represented with the ConstantFP class, which holds
// the numeric value in a APFloat internally (APFloat has the capability of holding floating point
// constants of Arbitrary Precisions).  This code basically just creates and returns a ConstantFP.
// Note that in the LLVM IR that constants are all uniqued together and shared. For this reason, the API
// uses the "foo::get(...)" instead of the "new foo(...)" or "foo::Create()".
//
Value* NumberExprAST::codegen() {
  return ConstantFP::get(getGlobalContext(), APFloat(val));
}

class VariableExprAST : public ExprAST {
  std::string Name;
  public:
  VariableExprAST(const std::string& name) : Name(name) {}
  Value* codegen() ;
};

Value* VariableExprAST::codegen() {
  Value* v = NamedValues[Name];
  if (!v) ErrorV("Unknown variable name");
  return v;
}

class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;
  public:
  BinaryExprAST(char op, 
                std::unique_ptr<ExprAST> LHS, 
                std::unique_ptr<ExprAST> RHS) : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  Value* codegen() ;
};

Value* BinaryExprAST::codegen() {
  Value *L = LHS->codegen();
  Value* R = RHS->codegen();
  if (!L || !R) return nullptr;

  switch(Op) {
    case '+':
      return Builder.CreateFAdd(L, R, "addtmp");
    case '-':
      return Builder.CreateFSub(L, R, "subtmp");
    case '*':
      return Builder.CreateFMul(L, R, "multmp");
    case '<': 
      L = Builder.CreateFCmpULT(L, R, "divtmp");
      return Builder.CreateUIToFP(L, Type::getDoubleTy(getGlobalContext()),
                                  "booltmp");
    default:
      return ErrorV("Invalid binary operator");
  }
}

class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;
  public:
    CallExprAST(const std::string& Callee, 
                std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}
    Value* codegen();
};

Value* CallExprAST::codegen() {
  Function* CalleeF = TheModule->getFunction(Callee);
  if(!CalleeF) return ErrorV("Unknown function reference");

  if(CalleeF->arg_size() != Args.size()) return ErrorV("Incorrect # arguments passed");

  std::vector<Value*> ArgsV;
  for(unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if(!ArgsV.back())
      return nullptr;
  }
  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
  public:
  PrototypeAST(const std::string&name, std::vector<std::string> args) :
    Args(std::move(args)), Name(name) {}
  Function* codegen();
  const std::string& getName() const { return Name; }
};

Function* PrototypeAST::codegen() {
  std::vector<Type*> Doubles(Args.size(), Type::getDoubleTy(getGlobalContext()));
  FunctionType* FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()), Doubles, false);

  Function* F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());
  unsigned index = 0;
  for(auto & arg: F->args())
    arg.setName(Args[index++]);

}

class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;
  public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body) :
    Proto(std::move(Proto)), Body(std::move(Body)) {}
  Function* codegen();
};

Function* FunctionAST::codegen() {
  Function* TheFunction = TheModule->getFunction(Proto->getName());

  if(!TheFunction) TheFunction = Proto->codegen();

  if(!TheFunction) return nullptr;

  if(!TheFunction->empty()) return (Function*)ErrorV("Function cannot be redefined.");

  BasicBlock* BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  NamedValues.clear();
  for(auto & arg : TheFunction->args())
    NamedValues[arg.getName()] = &arg;

  if(Value* RetVal = Body->codegen()) {
    Builder.CreateRet(RetVal);

    verifyFunction(*TheFunction);
    return TheFunction;
  }

  TheFunction->eraseFromParent();
  return nullptr;
}


} // end of namespace

//===----------------------------------------------------------------------===//
//// Parser
////===----------------------------------------------------------------------===//
//
///// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
///// token the parser is looking at.  getNextToken reads another token from the
///// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() {
  return CurTok = gettok();
}

std::unique_ptr<PrototypeAST> ErrorP(const char* Str) {
  Error(Str);
  return nullptr;
}

// numberexprt ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto result = helper::make_unique<NumberExprAST>(NumVal);
  getNextToken();
  return std::move(result);
}

static std::unique_ptr<ExprAST> ParseExpression();

// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken();
  auto v = ParseExpression();
  if (!v) return nullptr;

  if (CurTok != ')') 
    return Error("expected ')'");
  getNextToken(); 
  return v;
}

// identifierexpression
//  ::= identifier
//  ::= identifier '(' expression* ')'
//
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string idName = IdentifierStr;

  getNextToken();

  if(CurTok != '(') return helper::make_unique<VariableExprAST>(idName);

  // call
  getNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if(CurTok != ')') {
    while(1) {
      if(auto Arg = ParseExpression()) 
        Args.push_back(std::move(Arg));
      else 
        return nullptr;

      if (CurTok == ')')
        break; 

      if (CurTok != ',')
        return Error("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  getNextToken(); // eat )
  return helper::make_unique<CallExprAST>(idName, std::move(Args));
}

// primary
// ::= identifierexpr
// ::= numberexpr
// ::= parenexpr
//
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch(CurTok) {
    default:
      return Error("unknown token when expecting an expression");
    case tok_identifier:
      return ParseIdentifierExpr();
    case tok_number:
      return ParseNumberExpr();
    case '(':
      return ParseParenExpr();
  }
}

// binopprecedence - this holds the precedence for each binary operator 
// that is defined.
static std::map<char, int> BinopPrecedence;

static int GetTokPrecedence() {
  if (!isascii(CurTok)) return -1;

  // make sure it is a declared binop
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0) return -1;
  return TokPrec;
}

// binoprhs
// ::= ( '+' primary )*
//
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
  // if this is a binop, find its precedence
  while(1) {
    int tokprec = GetTokPrecedence();
    if (tokprec < ExprPrec) return LHS;

    int BinOp = CurTok;
    getNextToken();

    auto RHS = ParsePrimary();
    if (!RHS) return nullptr;

    int nextprec = GetTokPrecedence();
    if (tokprec < nextprec) {
      RHS = ParseBinOpRHS(tokprec + 1, std::move(RHS));
      if (!RHS) return nullptr;
    }

    // merge lhs/rhs
    LHS = helper::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

// expression
// ::= primary binoprhs
//
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS) return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}


/// prototype
/////   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  if(CurTok != tok_identifier) return ErrorP("Expected function name in prototype");

  std::string fnName = IdentifierStr;
  getNextToken();

  if (CurTok!= '(') return ErrorP("Expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while(getNextToken() == tok_identifier) 
    ArgNames.push_back(IdentifierStr);
  if(CurTok!= ')')
    return ErrorP("Expected ')' in prototype");

  getNextToken(); // eat )

  return helper::make_unique<PrototypeAST>(fnName, std::move(ArgNames));
}

// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
  getNextToken();
  auto Proto = ParsePrototype();
  if(!Proto) return nullptr;

  if(auto E = ParseExpression()) 
    return helper::make_unique<FunctionAST>(std::move(Proto), std::move(E));

  return nullptr;
}

//external ::= 'extern' definition
static std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken();
  return ParsePrototype();
}


// toplevelexpression ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if(auto E = ParseExpression()) {
    auto Proto =helper::make_unique<PrototypeAST>("", std::vector<std::string>());
    return helper::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
//// Top-Level parsing
////===----------------------------------------------------------------------===//
//

static void HandleDefinition() {
  if (auto f = ParseDefinition()) {
    if(auto g = f->codegen()) {
      fprintf(stderr, "Read a function definition.\n");
      g->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto f = ParseExtern()) {
    if(auto g = f->codegen()) {
      fprintf(stderr, "Parsed an extern\n");
      g->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto f = ParseTopLevelExpr()) {
    if (auto g = f->codegen()) {
      fprintf(stderr, "Parsed a top-level expr\n");
      g->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

// top ::= definition | external | expression | ';'
static void MainLoop() {
  while(1) {
    fprintf(stderr, "ready> ");
    switch(CurTok) { 
      case tok_eof:
        return;
      case ';':
        getNextToken();
        break;
      case tok_def:
        HandleDefinition();
        break;
      case tok_extern:
        HandleExtern();
        break;
      default:
        HandleTopLevelExpression();
        break;
    }
  }
}



int main() {
  // precedence of the operators is organized
  // around having non-zero values
  // with 10 being the lowest and 40 being the 
  // highest.
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 30;
  BinopPrecedence['*'] = 40;

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  TheModule = llvm::make_unique<Module>("my cool jit", getGlobalContext());

  // Run the main "interpreter loop" now.
  MainLoop();

  TheModule->dump();

  return 0;
}

