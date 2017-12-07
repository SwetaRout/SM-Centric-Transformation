/* This example shows how to find cuda kernel calls in an input .cu file */
#include<stdio.h>
#include<stdlib.h>
#include<sstream>
#include<string>
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Rewrite/Core/Rewriter.h"
using namespace std;
using namespace clang::tooling;
using namespace llvm;
using namespace clang;
using namespace clang::ast_matchers;
Rewriter rewriter;


// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
struct ChildInfo{
	string fName;
	string inlineStr;
	SourceRange child_sr;
	SourceLocation child_loc;
	SourceRange childRet_sr;
	bool childGlobal = false;
	string loopVar;
};
struct KernelInfo{
	string tmp;
	SourceRange kernelRange;
};
int countChild = 0;
int countKernel = 0;
vector<ChildInfo> childInfo;
static cl::extrahelp MoreHelp("\nMore help text...");
vector<KernelInfo> kernelInfo;
CharSourceRange sr_updateId;
SourceRange parentLoc;
string childKName;

static std::string numOfblocksNthreads="";
class CudaCallPrinter : public MatchFinder::MatchCallback {
public :
 std::string fl_Replacements(string str,string oldStr,string newStr= "",string separator = "")
	         {
			                 size_t sPos=0;
					                 while((sPos = str.find(oldStr, sPos)) != std::string::npos)
								                 {
											                         int len = 0;
														                         if(separator == "")
																		                                 len = oldStr.length();
																	                         else
																					                         {
																									                                 for(unsigned int l=sPos;l<str.length();l++)
																														                                 {
																																			                                         if(str[l]==';')                                                                                                                                                                                                         break;
																																								                                         len=len+1;
																																}                         }																		       str.replace(sPos,len,newStr);				                 }
        return str;
									         }

	int getLengthOfKernelCall(std::string in){
		string finalstr;
		for(int i=in.length()-1;i>=0;i--)
		{
			if(in[i]==':')
			{
				string lengthStr;
				for(int j = finalstr.length()-1;j>=0;j--)
				{
					lengthStr=lengthStr+finalstr[j];
				}
				return atoi(lengthStr.c_str());
			}
			finalstr=finalstr+in[i];
		}
		return 0;

	}
	std::string stripOffhex(std::string in) {
	      string finalstr;
	      bool addFinal=false;
	          for(unsigned int i = 0; i < in.length(); i++) {
			  	  if(in[i]=='{')
					 addFinal=true;
				  if(addFinal)
				         finalstr = finalstr+in[i];

				      }
		      return finalstr;
        }

  virtual void run(const MatchFinder::MatchResult &Result) {
	  const Stmt *K1 = Result.Nodes.getNodeAs<clang::Stmt>("childCall");
	    if(K1)
	    {
		    bool found = false;
		    parentLoc = K1->getSourceRange();
		    llvm::outs()<<"\n**************loc  "<<parentLoc.getBegin().printToString(Result.Context->getSourceManager())<<"\n";
		    numOfblocksNthreads = "";
		    getNumOfBlocksNThreads(K1,Result.Context);
		    for(Stmt::const_child_iterator l1=K1->child_begin();l1!=K1->child_end();++l1)
		    {
			if(isa<ImplicitCastExpr>(*l1))
			{
				for(Stmt::const_child_iterator l2=l1->child_begin();l2!=l1->child_end();++l2)
				{
					if(isa<DeclRefExpr>(*l2))
					{
						if(!found)
						{
							found = true;
							const Stmt *kNmStmt = *l2;
							llvm::outs()<<"\n********* loc **********\n"<<kNmStmt->getSourceRange().getBegin().printToString(Result.Context->getSourceManager());
					       	        const DeclRefExpr *e1 = cast<DeclRefExpr>(kNmStmt);
							childKName = e1->getNameInfo().getAsString();
						}
					}
				}
			}
		    }
		    llvm::outs()<<"child name :"<<childKName;

	    }

    const Stmt *S1 = Result.Nodes.getNodeAs<clang::Stmt>("functionDecl");
    ASTContext *Context = Result.Context;
    if(S1 && isa<DeclRefExpr>(*S1))
    {
          const DeclRefExpr *e = cast<DeclRefExpr>(S1);
	  SourceRange kernelRange;
	  string tmp;
	  CharSourceRange sr_updateId;
	  for(unsigned int kr=0;kr<kernelInfo.size();++kr)
	  {
		  if(kernelInfo[kr].tmp == e->getNameInfo().getAsString())
		  {
			  kernelRange = kernelInfo[kr].kernelRange;
			  tmp = kernelInfo[kr].tmp;
		  }
	  }


          if(e->getNameInfo().getAsString()==tmp)
	  {
		 //llvm::outs()<<e->getNameInfo().getAsString()<<"\n";
		  const FunctionDecl *F = e->getDecl()->getAsFunction();
		 // Stmt *functionBody = F->getBody();
		  string inlineStr;
		  SourceRange child_sr;
		  SourceLocation child_loc;
		  SourceRange childRet_sr;
		  bool childGlobal = false;
		  string loopVar;
		  for(unsigned int i = 0;i<childInfo.size();++i)
		  {
		     if(childInfo[i].fName == childKName)
		     {
			     llvm::outs()<<"\n ......here....... \n";
		     	inlineStr =childInfo[i].inlineStr;
		        child_sr =childInfo[i] .child_sr;
		        child_loc =childInfo[i] .child_loc;
		        childRet_sr =childInfo[i] .childRet_sr;
				childGlobal = childInfo[i].childGlobal;
				loopVar = childInfo[i].loopVar;
		      }
		  }
		  bool invalid1;
		  CharSourceRange conditionRange = CharSourceRange::getTokenRange(parentLoc.getBegin(), parentLoc.getEnd());
		  StringRef str1 = Lexer::getSourceText(conditionRange, Context->getSourceManager(), Context->getLangOpts(), &invalid1);
		  string s=str1;
                  if(childGlobal)
		  	rewriter.RemoveText(childRet_sr.getBegin().getLocWithOffset(-10),11);
		   bool invalid;
		   CharSourceRange funcRange = CharSourceRange::getTokenRange(child_loc, child_sr.getEnd());
		   StringRef str = Lexer::getSourceText(funcRange, Context->getSourceManager(), Context->getLangOpts(), &invalid);
		   string tmpStr = str;
		   rewriter.RemoveText(child_loc,tmpStr.length());
           rewriter.RemoveText(childRet_sr);
		   std::string add_before ="int FL_childGridThreads = "+numOfblocksNthreads+";\n\tfor (unsigned "+loopVar+"= 0;"+loopVar+" < FL_childGridThreads; ++"+loopVar+")\n" ;
		   inlineStr = add_before+inlineStr;

		   rewriter.ReplaceText(parentLoc.getBegin(),s.length(),inlineStr);
		   rewriter.overwriteChangedFiles();
		 }
    }
    const Stmt *chF = Result.Nodes.getNodeAs<clang::Stmt>("childFunc");
    if(chF)
    {
	    const DeclRefExpr *e2 = cast<DeclRefExpr>(chF);
	    const FunctionDecl *F2 = e2->getDecl()->getAsFunction();
	    for(unsigned int kr=0;kr<kernelInfo.size();++kr)
	    {
		    if(kernelInfo[kr].tmp == e2->getNameInfo().getAsString())
		    {
	//		const DeclRefExpr *e2 = cast<DeclRefExpr>(chF);
	//		const FunctionDecl *F2 = e2->getDecl()->getAsFunction();
				Stmt *functionBody = F2->getBody();
 	              string threadIDcalc = "";
                  string lVar;
                  for(Stmt::const_child_iterator i1=functionBody->child_begin();i1!=functionBody->child_end();++i1)
                  {
                  bool invalid2;
                  CharSourceRange conditionRange = CharSourceRange::getTokenRange((i1->getSourceRange()).getBegin(), (i1->getSourceRange()).getEnd());
                  StringRef str1 = Lexer::getSourceText(conditionRange, Context->getSourceManager(), Context->getLangOpts(), &invalid2);

                  string f = str1;
//                size_t pos = str1.find("threadIdx.x");
                  if(str1.find("threadIdx.x") != std::string::npos)
                  {
                          threadIDcalc = f;
                          bool found=false,start=false;
                          for(int i=f.length()-1;i>=0;i--)
                          {
                                  if(start && found && f[i]==' ')
                                          break;
                                  if(start)
                                  {
                                          found = true;
                                          lVar = lVar+f[i];
                                  }
                                  if(f[i] == '=')
                                  {
                                          start = true;
                                  }
                          }
						  reverse(lVar.begin(),lVar.end());
                  }
            }


		  stringstream SSAfter;
		  SSAfter<<F2->getBody();
		  LangOptions LangOpts=Context->getLangOpts();
		  PrintingPolicy Policy(LangOpts);
		  std::string s2;
		  llvm::raw_string_ostream as(s2);
		  functionBody->printPretty(as,0,Policy);
		  SSAfter<<as.str()<<"\n";
		  std::string fBody = SSAfter.str();
		  fBody = fl_Replacements(fBody,"unsigned int","unsigned");
		  if(threadIDcalc != "")
          {
                	  std::size_t found = fBody.find("unsigned "+lVar);
                 int length = 0;

		 for(int i  = found;fBody[i]!=';';i++){
		 	length++;	
						 	}
                 if (found!=std::string::npos){
                        fBody.erase(found,length+1);
                 }
                 else
                         llvm::outs() << "ThreadID calc occurence not found\n";
          }
		 
		  fBody = stripOffhex(fBody);
		  fBody = fl_Replacements(fBody,"return","continue",";");
		  childInfo.push_back(ChildInfo());
          childInfo[countChild].loopVar = lVar;
          childInfo[countChild].inlineStr=fBody;
          childInfo[countChild].child_sr=F2->getSourceRange();
          childInfo[countChild].child_loc = F2->getLocation();
		  childInfo[countChild].childRet_sr = F2->getReturnTypeSourceRange();
		  childInfo[countChild].childGlobal = false;
		  if(F2->isGlobal())
			  childInfo[countChild].childGlobal = true;
		  childInfo[countChild].fName = e2->getNameInfo().getAsString();
                  countChild++;
		    }
	    }
    }


    const Stmt *S = Result.Nodes.getNodeAs<clang::Stmt>("cudaCall");
    if(!S)
	    return;
    Stmt::const_child_iterator i,j;
    j=S->child_end();
    for(i=S->child_begin();i!=j;++i)
    {
	    if(isa<ImplicitCastExpr>(*i))
	    {
		    for(Stmt::const_child_iterator k=i->child_begin(),l=i->child_end();k!=l;++k)
		    {
			    if(isa<DeclRefExpr>(*k))
			    {
				  const Stmt *kNmStmt = *k;
				  const DeclRefExpr *e1 = cast<DeclRefExpr>(kNmStmt);
				  kernelInfo.push_back(KernelInfo());
				  kernelInfo[countKernel].tmp=e1->getNameInfo().getAsString();
				  kernelInfo[countKernel].kernelRange = S->getSourceRange();
				  countKernel++;
			    }
		    }
	     }
    }
  }
  void getNumOfBlocksNThreads(const Stmt *S,ASTContext *Context)
  {
	  if(!S)
		  return;
      Stmt::const_child_iterator i,j;
      int count = 0;
      j=S->child_end();
      std::string s;
      for(i=S->child_begin();i!=j;++i)
      {
               if(isa<CallExpr>(*i))
	        {
		    for(Stmt::const_child_iterator k=i->child_begin(),l=i->child_end();k!=l;++k)
		    {
			    if(isa<CXXConstructExpr>(*k))
			    {
				    for(Stmt::const_child_iterator k1=k->child_begin(),l1=k->child_end();k1!=l1;++k1)
				    {
				          if(isa<MaterializeTemporaryExpr>(*k1))
					  {
						  for(Stmt::const_child_iterator ch1=k1->child_begin();ch1!=k1->child_end();++ch1)
						  {
							  if(isa<ImplicitCastExpr>(*ch1))
							  {
								  for(Stmt::const_child_iterator ch2=ch1->child_begin();ch2!=ch1->child_end();++ch2)
								  {
									  if(isa<ImplicitCastExpr>(*ch2))
									  {
										  for(Stmt::const_child_iterator ch3=ch2->child_begin();ch3!=ch2->child_end();++ch3)
										  {
											  if(isa<CXXConstructExpr>(*ch3))
											  {
												  for(Stmt::const_child_iterator ch4=ch3->child_begin();ch4!=ch3->child_end();++ch4)
												  {
													  if(isa<BinaryOperator>(*ch4)||isa<ParenExpr>(*ch4))
													  {
													  	bool invalid;
														CharSourceRange conditionRange = CharSourceRange::getTokenRange(ch4->getLocStart(), ch4->getLocEnd());														                                                                                                                                      StringRef str = Lexer::getSourceText(conditionRange, Context->getSourceManager(), Context->getLangOpts(), &invalid);
					 											  if(invalid)
																  {
																	  llvm::outs()<<"INVALID";
																  }
													       	std::string str1=str;
													        std::size_t found = str1.find("/");
													       	if (found!=std::string::npos)
														{
															if(isa<ParenExpr>(*ch4))
														       	{
																 str1 = "(int)"+str1;
														     	}
															else
															{
																str1 = "(int)("+str1+")";
															}
														}
														if(count == 1 && isa<BinaryOperator>(*ch4))
														{
															str1 = "("+str1+")";
														}
													       	numOfblocksNthreads = numOfblocksNthreads+str1;
														if(count == 0)
														{
															numOfblocksNthreads = numOfblocksNthreads + "*";
															count = 1;
														}
													  }
													  if(isa<ImplicitCastExpr>(*ch4))
													  {
														  for(Stmt::const_child_iterator ch5=ch4->child_begin();ch5!=ch4->child_end();++ch5)
														  {

															  if(isa<IntegerLiteral>(*ch5))
															  {
																const Stmt *threadsNblocks = *ch5;
																const IntegerLiteral *numOfThreads = cast<IntegerLiteral>(threadsNblocks);
																double val = numOfThreads->getValue().signedRoundToDouble();
																int t = (int)val;
																stringstream ss;
																ss<<t;
																numOfblocksNthreads =numOfblocksNthreads + ss.str();
																if(count == 0)
																{
																	numOfblocksNthreads = numOfblocksNthreads + "*";
																	count=1;
																}
															  }
														  }
													  }
												  }
											  }
										  }
									  }
								  }
							  }
						  }
					  }
				    }
			    }
		    }

	      }
	  }
      }

};

class MyASTConsumer: public ASTConsumer {
public:
  MyASTConsumer () {
    Finder.addMatcher(CudaCallMatcher, &cudaPrinter);
    Finder.addMatcher(FunctionDeclMatcher, &cudaPrinter);
    Finder.addMatcher(ChildFunctionDefMatcher, &cudaPrinter);
  }
  void HandleTranslationUnit(ASTContext &Context) override {
    // Run the matchers when we have the whole TU parsed.
    rewriter.setSourceMgr(Context.getSourceManager(),Context.getLangOpts());
    Finder.matchAST(Context);
  }

private:
  CudaCallPrinter cudaPrinter;
  StatementMatcher CudaCallMatcher= cudaKernelCallExpr().bind("cudaCall");
  StatementMatcher FunctionDeclMatcher = declRefExpr(to(functionDecl(hasDescendant(cudaKernelCallExpr().bind("childCall"))))).bind("functionDecl");
  StatementMatcher ChildFunctionDefMatcher = declRefExpr(to(functionDecl())).bind("childFunc");
  MatchFinder Finder;
};

// For each source file provided to the tool, a new FrontendAction is created.
class MyFrontendAction : public ASTFrontendAction {
public:
  MyFrontendAction() {}
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
						 StringRef file) override {
    return llvm::make_unique<MyASTConsumer>();
  }
};


int main(int argc, const char **argv) {
  CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
		 OptionsParser.getSourcePathList());

  int res = Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
  //rewriter.getEditBuffer(rewriter.getSourceMgr().getMainFileID()).write(errs());
  //rewriter.overwriteChangedFiles();
  return res;
}
