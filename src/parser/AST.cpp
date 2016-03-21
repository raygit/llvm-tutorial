#include <memory>
#include <string>
#include <vector>
#include <map>
#include <cctype>
#include <cstdio>
#include <cstdlib>

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

class ExprAST {
  public:
    virtual ~ExprAST() {}
};

class NumberExprAST : public ExprAST {
private:
  double val;
public:
  NumberExprAST(double val) : val(val) {}
};


class VariableExprAST : public ExprAST {
  std::string Name;
  public:
  VariableExprAST(const std::string& name) : Name(name) {}
};

class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;
  public:
  BinaryExprAST(char op, 
                std::unique_ptr<ExprAST> LHS, 
                std::unique_ptr<ExprAST> RHS) : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;
  public:
    CallExprAST(const std::string& Callee, 
                std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}
};

class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
  public:
  PrototypeAST(const std::string&name, std::vector<std::string> args) :
    Args(std::move(args)), Name(name) {}
};

class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;
  public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body) :
    Proto(std::move(Proto)), Body(std::move(Body)) {}
};
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

std::unique_ptr<ExprAST> Error(const char* Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
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
  if (ParseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (ParseExtern()) {
    fprintf(stderr, "Parsed an extern\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (ParseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expr\n");
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
  
  // Run the main "interpreter loop" now.
  MainLoop();
}

