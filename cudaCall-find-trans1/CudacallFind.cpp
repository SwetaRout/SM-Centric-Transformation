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
struct VarDeclInfo{
	SourceRange sr;
	string varName;
	string varType;
};
struct ChildInfo{
	string fName;
	string inlineStr;
	SourceRange child_sr;
	SourceLocation child_loc;
	SourceRange childRet_sr;
	bool childGlobal = false;
};
struct KernelInfo{
	string tmp;
	SourceRange kernelRange;
};
static cl::extrahelp MoreHelp("\nMore help text...");

static int countVar = 0;
vector<KernelInfo> kernelInfo;
vector<ChildInfo> childInfo;
SourceRange updateparent_sr;
SourceRange parentLoc;
int countChild = 0;
int countKernel = 0;
vector<SourceRange> sr_returnStmts;
vector<VarDeclInfo> varDeclInfo;
std::string phase1;
std::string phase2;
static std::string numOfblocksNthreads="";
static std::string numOfblocks = "";
static std::string numOfthreads = "";
string childKName;
class CudaCallPrinter : public MatchFinder::MatchCallback {
public :
	// replace oldStr occurences from str with newStr. 
	//Oldstr length s calculated upto the first occurence of separatorif it is not ""
	std::string fl_Replacements(string str,string oldStr,string newStr = "",string separator = "")
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
					if(str[l]==';')																			                                           		break;
	        		        len=len+1;
				}
			}
		       	str.replace(sPos,len,newStr);
		}
		return str;
	}
	//find the length of kernel call statement from the kernel source location statement
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
	/* This function is used to remove all extra characters at the beginning of function body*/
	std::string stripOffhex(std::string in) {
	      string finalstr;
	      bool addFinal=false;
	          for(unsigned int i = 0; i < in.length()-1; i++) {
			if(addFinal)
	                         finalstr = finalstr+in[i];
			  if(in[i]=='{')
					 addFinal=true;
				 
				      }
		  finalstr.erase(finalstr.rfind("}"),1);
		      return finalstr;
  	}
  virtual void run(const MatchFinder::MatchResult &Result) {
   const Stmt *K1 = Result.Nodes.getNodeAs<clang::Stmt>("childCall"); //find all child kernel calls
	    if(K1)
	    {
		    bool found = false;
		    parentLoc = K1->getSourceRange();
			updateparent_sr = K1->getSourceRange();;
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
    ASTContext *Context = Result.Context;
    const VarDecl *IncVar = Result.Nodes.getNodeAs<VarDecl>("varDecl");// find all variable declarations
    if(IncVar)
    {
	    const QualType qt = IncVar->getType();
	    varDeclInfo.push_back(VarDeclInfo());
	    varDeclInfo[countVar].sr=IncVar->getSourceRange();
	    varDeclInfo[countVar].varName=IncVar->getName().str();
	    varDeclInfo[countVar].varType=qt.getAsString();
	    countVar++;
    }
    const Stmt *S2 = Result.Nodes.getNodeAs<clang::Stmt>("RetStmt");
    if(S2)
    {
        sr_returnStmts.push_back(S2->getSourceRange());
    }
    const Stmt *S1 = Result.Nodes.getNodeAs<clang::Stmt>("functionDecl");
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
		  llvm::outs()<<e->getNameInfo().getAsString()<<"\n";
		 // bool isChild = true;
		  const FunctionDecl *F = e->getDecl()->getAsFunction();
		  Stmt *functionBody = F->getBody();
		  string inlineStr;
		  SourceRange child_sr;
		  SourceLocation child_loc;
		  SourceRange childRet_sr;
		  bool childGlobal = false;
		  for(unsigned int i = 0;i<childInfo.size();++i) // retrieve all the child function info
		  {
		     if(childInfo[i].fName == childKName)
		     {
			     llvm::outs()<<"\n ......here....... \n";
		     	inlineStr =childInfo[i].inlineStr;
				phase2 = inlineStr;
		        child_sr =childInfo[i] .child_sr;
		        child_loc =childInfo[i] .child_loc;
		        childRet_sr =childInfo[i] .childRet_sr;
			    childGlobal = childInfo[i].childGlobal;
		      }
		  }

		  	SourceRange fBody_sourceRange = functionBody->getSourceRange();

			  //Free Launch transformations at the parent call statement
			  SourceLocation loc = F->getLocEnd().getLocWithOffset(-1);
			  string paramDecl = "int blocks,char *FL_Args";
			  if(F->getNumParams()>0)
			  {
				  const ParmVarDecl *Parm = F->getParamDecl(F->getNumParams()-1);
				  loc = Parm->getSourceRange().getEnd().getLocWithOffset(5);
				  paramDecl = ","+paramDecl;
			  }
			  rewriter.InsertText(loc,paramDecl,true,true);
			  rewriter.InsertText(kernelRange.getBegin().getLocWithOffset(-1),"\n\tchar *FL_Arguments; cudaMalloc((void **)&FL_Arguments,MAX_FL_ARGSZ);\n\tcudaMemset(FL_Arguments,0,MAX_FL_ARGSZ);\n",true,true);
			  rewriter.InsertText(kernelRange.getEnd(),",kconf.getNumberOfBlocks(),FL_Arguments",true,true);
			  rewriter.InsertText(kernelRange.getEnd().getLocWithOffset(1),";\n\tcudaFree(FL_Arguments)",true,true);
			  //Free Launch transformations in the parent call function
			  vector<SourceRange>::iterator it;
			  rewriter.InsertText(fBody_sourceRange.getBegin().getLocWithOffset(1),"\n FL_T1_Preloop;\n",true,true);
			  string lVar = "";
              SourceRange th_sr;
              int th_l;
              for(Stmt::const_child_iterator i1=functionBody->child_begin();i1!=functionBody->child_end();++i1)
              {
	              bool invalid2;
	              CharSourceRange conditionRange = CharSourceRange::getTokenRange((i1->getSourceRange()).getBegin(), (i1->getSourceRange()).getEnd());
	              StringRef str1 = Lexer::getSourceText(conditionRange, Context->getSourceManager(), Context->getLangOpts(), &invalid2);
	              string f = str1;

	              if(str1.find("threadIdx.x") != std::string::npos && str1.find("blockIdx.x") != std::string::npos && str1.find("blockDim.x") != std::string::npos)
	              {
	                      th_sr = i1->getSourceRange();
	                      bool found=false,start=false;
	                      th_l = f.length();
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

              if(lVar!="")
              {
                    rewriter.ReplaceText(th_sr.getBegin(),th_l,"unsigned "+lVar+"= (blockIdx.x+FL_y) * blockDim.x + threadIdx.x;\n"); // replace threadid calculation for parent call
              }
			  if(sr_returnStmts.empty())
				  return;
			  for(it = sr_returnStmts.begin();it!=sr_returnStmts.end();++it) // iterate to find all return statements info
			  {
				assert((*it).isValid() && "Source range is invalid");
			        SourceLocation begin = (*it).getBegin();
				SourceLocation end = (*it).getEnd();
				if((Context->getSourceManager()).isPointWithin(begin,fBody_sourceRange.getBegin(),fBody_sourceRange.getEnd()) && (Context->getSourceManager()).isPointWithin(end,fBody_sourceRange.getBegin(),fBody_sourceRange.getEnd())) //find all return statements within the parent function call range
				{
					bool invalid;
					CharSourceRange conditionRange = CharSourceRange::getTokenRange(begin, end);
					StringRef str = Lexer::getSourceText(conditionRange, Context->getSourceManager(), Context->getLangOpts(), &invalid);
					if(!invalid)
					{
						string str1 = str;
						int l = str1.length();
				                rewriter.ReplaceText(begin,l,"continue");
					}

				}
			  }
			  string argSzPhase1 = "FL_childKernelArgSz = sizeof(int)+";
			  string memCpyPhase1;
			  string argMemCpyPhase2;
			  string tmp_;
			  for(unsigned int i=0;i<varDeclInfo.size();++i)
			  {
				  SourceRange sr_tmp = varDeclInfo[i].sr;
				  SourceLocation begin = sr_tmp.getBegin();
			      SourceLocation end = sr_tmp.getEnd();
				  if((Context->getSourceManager()).isPointWithin(begin,fBody_sourceRange.getBegin(),fBody_sourceRange.getEnd()) && (Context->getSourceManager()).isPointWithin(end,fBody_sourceRange.getBegin(),fBody_sourceRange.getEnd())) // find all the variables that are declared within the  parent kernel function range
				  {
					  argSzPhase1 = argSzPhase1+"sizeof("+varDeclInfo[i].varType+")+"; // calculate the size of all variables used
					  memCpyPhase1 = memCpyPhase1+"\tmemcpy((void*)_tmp_p, (void*) &"+varDeclInfo[i].varName+", sizeof("+varDeclInfo[i].varType+"));\n\t"; // memory assigned to all variables used
					  argMemCpyPhase2 = argMemCpyPhase2+varDeclInfo[i].varType+" "+varDeclInfo[i].varName+";\n\tmemcpy((void*)&"+varDeclInfo[i].varName+", (void*)_tmp_p, sizeof("+varDeclInfo[i].varType+"));\n\t";

					  string type = varDeclInfo[i].varType;
						if(varDeclInfo[i].varType == "foru")
						{
							type="unsigned";
						}
						if(type == "_Bool")
							type = "bool";

					  	tmp_ = "_tmp_p+=sizeof("+type+");\n\t";
					  	memCpyPhase1 = memCpyPhase1+tmp_;
					  	argMemCpyPhase2 = argMemCpyPhase2+tmp_;
				  }
			  }

			  memCpyPhase1.erase(memCpyPhase1.rfind(tmp_),tmp_.length());
			  argMemCpyPhase2.erase(argMemCpyPhase2.rfind(tmp_),tmp_.length());
			  if(!argSzPhase1.empty())
			  {
				  argSzPhase1.erase( argSzPhase1.size() - 1 );
			  }
			  phase1 = "\n\tint FL_lc = atomicAdd(&FL_count,1);\n\t"+argSzPhase1+";\n\tchar * _tmp_p = (char *) ((&FL_Args[0])+FL_lc*FL_childKernelArgSz);\n\tint _tmp_childGridSize = "+numOfblocks+";\n\tmemcpy((void*)_tmp_p, (void*) &_tmp_childGridSize, sizeof(int));\n\t_tmp_p+=sizeof(int);\n\tFL_childBlockSize="+numOfthreads+";\n"+memCpyPhase1;
			  phase1=phase1+"\n\tatomicAdd(&FL_totalBlocks,_tmp_childGridSize);\n\tFL_check = FL_ttid;\n\tgoto P;\n\tC:   __threadfence();\n";
			 bool invalid1;
		      CharSourceRange conditionRange = CharSourceRange::getTokenRange(parentLoc.getBegin(), parentLoc.getEnd());
		      StringRef str1 = Lexer::getSourceText(conditionRange, Context->getSourceManager(), Context->getLangOpts(), &invalid1);
		      string s=str1;
// add the strings needed for transformation
			  int l = s.length();
			  phase1 = fl_Replacements(phase1,"unsigned int","unsigned");
			  phase1 = fl_Replacements(phase1,"_Bool","bool"); // the matcher gives bool as _BOOL. so these are changed

			  rewriter.ReplaceText(updateparent_sr.getBegin(),l,phase1);
			  phase2 = "\n\tchar * _tmp_p = (char*)((&FL_Args[0])+ckernelSeqNum*FL_childKernelArgSz);\n\t_tmp_p+=sizeof(int);" + argMemCpyPhase2 +phase2;
			  phase2="\nFL_T1_Postloop;"+phase2+"\nFL_postChildLog\n";
			  phase2 = fl_Replacements(phase2,"unsigned int","unsigned");
			  phase2 = fl_Replacements(phase2,"_Bool","bool");

			  rewriter.InsertText(fBody_sourceRange.getEnd().getLocWithOffset(-1),phase2,true,true);
			  bool invalid;
		      CharSourceRange funcRange = CharSourceRange::getTokenRange(child_loc, child_sr.getEnd());
	          StringRef str = Lexer::getSourceText(funcRange, Context->getSourceManager(), Context->getLangOpts(), &invalid);
	          string tmpStr = str;
              //rewriter.RemoveText(child_loc,tmpStr.length());
              //rewriter.RemoveText(childRet_sr);
		//	  if(childGlobal)
		  //			rewriter.RemoveText(childRet_sr.getBegin().getLocWithOffset(-10),11);
			  rewriter.overwriteChangedFiles(); // change the files after transformation
		  }
	}
 const Stmt *chF = Result.Nodes.getNodeAs<clang::Stmt>("childFunc"); // parse all function decl
    if(chF)
    {
	    const DeclRefExpr *e2 = cast<DeclRefExpr>(chF);
	    const FunctionDecl *F2 = e2->getDecl()->getAsFunction();
	    for(unsigned int kr=0;kr<kernelInfo.size();++kr)
	    {
		    if(kernelInfo[kr].tmp == e2->getNameInfo().getAsString()) // Find the function whose name matches with the kernel call name
		    {
				Stmt *functionBody = F2->getBody();
 	              string threadIDcalc = "";
                  string lVar;
                  for(Stmt::const_child_iterator i1=functionBody->child_begin();i1!=functionBody->child_end();++i1)
                  {
                  bool invalid2;
                  CharSourceRange conditionRange = CharSourceRange::getTokenRange((i1->getSourceRange()).getBegin(), (i1->getSourceRange()).getEnd());
                  StringRef str1 = Lexer::getSourceText(conditionRange, Context->getSourceManager(), Context->getLangOpts(), &invalid2);

                  string f = str1;
// find the variable that stores the thread id calculation
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

			// convert function body contents to a string
		  stringstream SSAfter;
		  SSAfter<<F2->getBody();
		  LangOptions LangOpts=Context->getLangOpts();
		  PrintingPolicy Policy(LangOpts);
		  std::string s2;
		  llvm::raw_string_ostream as(s2);
		  functionBody->printPretty(as,0,Policy);
		  SSAfter<<as.str()<<"\n";
		  std::string fBody = SSAfter.str();
	    
		  fBody = fl_Replacements(fBody,"unsigned int","unsigned"); // Sometimes fbody when parsed changes all unsigned to unsigned int. To make the code uniform and generic all unsigned int are converted to int if they are changed while parsing
	    if(threadIDcalc != "")
          {
                	  std::size_t found = fBody.find("unsigned "+lVar); // the threadid calculation variable is found to be lVar above and its type is unsigned, so unsigned lVar declaration statement is found from the function body and is modified  for the transformation
                 int length = 0;
		// calculate the length of the threadid calculation statement
		 for(int i  = found;fBody[i]!=';';i++){
		 	length++;	
						 	}
                 if (found!=std::string::npos){
                 	fBody.replace(found,length+1,"\nunsigned "+lVar+" = logicalChildBlockSeqNum * FL_childBlockSize +threadIdx.x%FL_childBlockSize;\n"); // modify the threadid calculations
                 }
                 else
                         llvm::outs() << "ThreadID calc occurence not found\n";
          }
		 
		  fBody = stripOffhex(fBody); // the function body comes  with some extra characters in the beginning. These characters are stripped off.
		  fBody = fl_Replacements(fBody,"return","continue",";");  // replace all return statements with continue in the child body
	  // Create child info for every child cernel call
		  childInfo.push_back(ChildInfo());
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

    const Stmt *S = Result.Nodes.getNodeAs<clang::Stmt>("cudaCall"); //Parse all the cuda kernel calls
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
  /*
  This function takes in the cuda kernel call statement and the given context as input
  This updates the num of blocks * num of  threads value expression as per the cuda kernel statement
  */
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
													  if(isa<BinaryOperator>(*ch4)||isa<ParenExpr>(*ch4)) //If the kernel call has an expression as child
													  {
													  	bool invalid;
														CharSourceRange conditionRange = CharSourceRange::getTokenRange(ch4->getLocStart(), ch4->getLocEnd());														                                                                                                                                      StringRef str = Lexer::getSourceText(conditionRange, Context->getSourceManager(), Context->getLangOpts(), &invalid);
					 											  if(invalid)
																  {
																	  llvm::outs()<<"INVALID";
																  }
																  std::string str1=str;
																  std::size_t found = str1.find("/");
																  if (found!=std::string::npos) //If there is a division operation in the expression convert it to int
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
																	numOfthreads=str1;
																	str1 = "("+str1+")";
																  }
																  numOfblocksNthreads = numOfblocksNthreads+str1;
																  if(count == 0)
																  {
																	  numOfblocks = str1;
																	  numOfblocksNthreads = numOfblocksNthreads + "*";  //The first parameter is num of blocks then add a "*"  and then append the nm of threads
																	  count=1;
																  }
															  }
													  if(isa<ImplicitCastExpr>(*ch4))
													  {
														  for(Stmt::const_child_iterator ch5=ch4->child_begin();ch5!=ch4->child_end();++ch5)
														  {

															  if(isa<IntegerLiteral>(*ch5)) //If the kernel call consists of integer literals
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
																	numOfblocks = ss.str();
																	numOfblocksNthreads = numOfblocksNthreads + "*";
																	count=1;
																}
																else
																{
																	numOfthreads = ss.str();
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
    Finder.addMatcher(RetStmtMatcher,&cudaPrinter);
    Finder.addMatcher(CudaCallMatcher, &cudaPrinter);
    Finder.addMatcher(FunctionDeclMatcher, &cudaPrinter);
    Finder.addMatcher(varDecl().bind("varDecl"),&cudaPrinter);
	Finder.addMatcher(ChildFunctionDefMatcher, &cudaPrinter);
  }
  void HandleTranslationUnit(ASTContext &Context) override {
    // Run the matchers when we have the whole TU parsed.
    rewriter.setSourceMgr(Context.getSourceManager(),Context.getLangOpts());
    FileID fID = Context.getSourceManager().getMainFileID();
    SourceLocation sl = Context.getSourceManager().getLocForStartOfFile(fID) ;
    rewriter.InsertText(sl,"#include \"freeLaunch_T1.h\"\n",true,true); // includes the new header file in the  given context
    Finder.matchAST(Context);
  }

private:
  CudaCallPrinter cudaPrinter; //Callback class for the matcher
  StatementMatcher CudaCallMatcher= cudaKernelCallExpr().bind("cudaCall");   //matcher to find all cuda kernel calls within the  given context
  StatementMatcher FunctionDeclMatcher = declRefExpr(to(functionDecl(hasDescendant(cudaKernelCallExpr().bind("childCall"))))).bind("functionDecl");  //matcher to find function declarations bound with "functionDecl" and parent kernel function calls  that have a kernel descendant bound with  "childCall"
  StatementMatcher ChildFunctionDefMatcher = declRefExpr(to(functionDecl())).bind("childFunc"); // matcher that finds all function declarations. this is used to find the child function body
 StatementMatcher RetStmtMatcher= returnStmt().bind("RetStmt"); //matcher to find all return statements
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

/*
function name : ReplaceString
param desc : string subject : the main string
		string search : to be found
		string replace : new string
return value : string - new string with old string replaced with new string
*/
std::string ReplaceString(std::string subject, const std::string& search,
		                          const std::string& replace) {
	    size_t pos = 0;
	        while ((pos = subject.find(search, pos)) != std::string::npos) {
			         subject.replace(pos, search.length(), replace);
				          pos += replace.length();
					      }
		    return subject;
}



int main(int argc, const char **argv) {
  CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
		 OptionsParser.getSourcePathList());

  int res = Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
      //Added the new header file
	 std::string fileName(argv[argc - 1]);
	std::string outName (fileName);
	size_t found;
	found = outName.find_last_of("/\\");
	std::string file = outName.substr(found+1);
	llvm::outs()<<file;
	outName = ReplaceString(outName, file, "freeLaunch_T1.h");   //Renamed the output file as given in the example
		llvm::errs() << "Output to: " << outName << "\n";
		 std::error_code OutErrorInfo;
		 std::error_code ok;
		 llvm::raw_fd_ostream outFile(llvm::StringRef(outName), OutErrorInfo, llvm::sys::fs::F_None);

		 if (OutErrorInfo == ok)
	{
				 outFile << "#ifndef __FreeLaunch_H\n#define __FreeLaunch_H\n\n";
				 outFile << "#define MAX_FL_ARGSZ (1024*1024*128)\n__device__ int FL_childBlockSize = 0;\n__device__ int FL_count= 0;\n__device__ uint FL__counter__ = 0;\n__device__ uint FL_childKernelArgSz = 0;\n__device__ void FL_syncAllThreads() {\n  __syncthreads();\n  uint to = gridDim.x-1;//getBlocksPerGrid() - 1;\n  if (threadIdx.x==0) {\n    volatile uint* counter = &FL__counter__;\n    if (atomicInc((uint*) counter, to) < to) {\n      while (*counter); // spinning...\n    }\n  }\n  __syncthreads();\n}\n__device__ int FL_totalBlocks=0;\n\n\n";
				 outFile << "#define FL_T1_Preloop \\\n  int FL_ttid = blockIdx.x*blockDim.x+threadIdx.x;\\\n  int FL_check = -1;\\\n  int FL_y=0;\\\n  volatile int *FL_pcount = &FL_count;\\\n  volatile int *FL_ptotalBlocks = &FL_totalBlocks;\\\n B: __threadfence();\\\n  if(FL_ttid == FL_check) {FL_check=-1;goto C;}\\\n  else if(FL_check == -2) goto P;\\\n  for(;FL_y+blockIdx.x<blocks; FL_y+=gridDim.x) \\\n    {//persistent threads loop\n\n";
				 outFile << "#define FL_T1_Postloop \\\n    }\\\n  FL_check = -2;\\\n P:FL_syncAllThreads();\\\n  if(*FL_pcount != 0  ){\\\n";
				 outFile << "    int ckernelSeqNum=0;int logicalChildBlockSeqNum=0;\\\n    int tasksPerParentThread = (*FL_ptotalBlocks+FL_childBlockSize*gridDim.x-1)/(FL_childBlockSize*gridDim.x); \\\n    for(int i=0;(i<tasksPerParentThread)&&(tasksPerParentThread*(FL_childBlockSize*blockIdx.x+threadIdx.x/FL_childBlockSize)+i<*FL_ptotalBlocks);i++) \\\n      { \\\n	int kernelSz;\\\n        memcpy((void*)&kernelSz, (void*)(&FL_Args[0]+ckernelSeqNum*FL_childKernelArgSz), sizeof(int)); \\\n";
				 outFile << "	if(i==0){ \\\n	  logicalChildBlockSeqNum = tasksPerParentThread*(FL_childBlockSize*blockIdx.x+threadIdx.x/FL_childBlockSize); \\\n	  while(logicalChildBlockSeqNum-kernelSz>=0) \\\n	    { \\\n	      logicalChildBlockSeqNum-=kernelSz; \\\n	      ckernelSeqNum++; \\\n              memcpy((void*)&kernelSz, (void*)(&FL_Args[0]+ckernelSeqNum*FL_childKernelArgSz), sizeof(int)); \\\n	    } \\\n	} \\\n	else{ \\\n";
				 outFile << "	  logicalChildBlockSeqNum +=1; \\\n	  if(logicalChildBlockSeqNum-kernelSz >= 0){ \\\n	    logicalChildBlockSeqNum-=kernelSz; \\\n	    ckernelSeqNum++; \\\n	  } \\\n	}\n\n\n";
				 outFile << "#define FL_postChildLog \\\n      } \\\n      FL_syncAllThreads(); \\\n      *FL_pcount=0; \\\n      *FL_ptotalBlocks=0; \\\n      FL_syncAllThreads(); \\\n      goto B; \\\n  } \n\n#endif	\n\n";
	}
	outFile.close();

  return res;
}
