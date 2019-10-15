
#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <cstdlib>
#define IN_ANAL_
#include "../../../include/dvm.h"
#undef IN_ANAL_
#include "../../../include/aks_structs.h"

#include <map>
#include <string>

using namespace std;
SgFile *current_file;
int current_file_id;
int RezervLabelForReturn = 1000;
const int MannyOfCopies = 1;


SgStatement * lastStmtOfDo(SgStatement *stdo)
{
	SgStatement *st;
	// second version  (change 04.03.08)
	st = stdo;

	st = st->lastNodeOfStmt();
	while ((st->variant() == FOR_NODE) || (st->variant() == WHILE_NODE))
	{
		st = st->lastNodeOfStmt();
	}
	if (st->variant() == LOGIF_NODE)
		return(st->lexNext());
	else
		return(st);
}

SgStatement * lastStmtOfIf(SgStatement *stif)
{
	SgStatement *st;
	// look for endif
	st = stif;
	st = st->lastNodeOfStmt();
	while ((st->variant() == ELSEIF_NODE))
	{
		st = st->lastNodeOfStmt();
	}
	return(st);
}

SgStatement * lastStmtOf(SgStatement *st)
{
	SgStatement *last;
	if (st->variant() == LOGIF_NODE)
		last = st->lexNext();
	else if ((st->variant() == FOR_NODE) || (st->variant() == WHILE_NODE))
		last = lastStmtOfDo(st);
	else if (st->variant() == IF_NODE || st->variant() == ELSEIF_NODE)
		last = lastStmtOfIf(st);
	else
		last = st->lastNodeOfStmt();
	return(last);
}

void TransferStatementBlock(SgStatement *first_st, SgStatement *out_st, SgStatement *st_end)
{
	SgStatement *st, *next;
	for (st = first_st; st != out_st; st = next)
	{
		out_st->unparsestdout();
		st->unparsestdout();
		st_end->unparsestdout();
		next = lastStmtOf(st)->lexNext();
		st->extractStmt(); 
		st_end->insertStmtBefore(*st, *st_end->controlParent());
	}
}

bool EndDoLoopChecker(SgFile *file)
{
	int funcNum = file->numberOfFunctions();
	bool checkOK = true;

	for (int i = 0; i < funcNum; ++i)
	{
		SgStatement *st = file->functions(i);
		SgStatement *lastNode = st->lastNodeOfStmt();

		while (st != lastNode)
		{
			if (st->variant() == CONTAINS_STMT)
				break;

			if (st->variant() == FOR_NODE)
			{
				SgForStmt *currSt = (SgForStmt*)st;
				if (currSt->isEnddoLoop() == 0)
				{
					checkOK = false;
				}
			}
			st = st->lexNext();
		}
	}
	return checkOK;
}

/*
void CommonParce(SgExprListExp *expr_list, map<string, SgSymbol*>& notchange_name)
{
	if (expr_list != nullptr) {
		SgSymbol *symb = expr_list->lhs()->symbol();
		if (symb != nullptr) {//+ проверка на наличие имени &&symb->identifier()
			switch (symb->variant())
			{
			default: {
				cout << "this COMMON dont coppy: " << symb->identifier() << "  variant =" << symb->variant() << endl << endl;
				notchange_name.insert(pair<string, SgSymbol*>{string(symb->identifier()), symb});
				break;
			}
			}
		}
		CommonParce(expr_list->next(), notchange_name);
	}
}
*/

void list_check(SgExpression *work_expr, map<string, SgSymbol*>& notchange_name) {

	if (work_expr == nullptr)
		return;
	switch (work_expr->variant())
	{
	case EXPR_LIST:
	{
		SgExprListExp *expr_list = isSgExprListExp(work_expr);
		if (expr_list != nullptr) {
			SgSymbol *symb = expr_list->lhs()->symbol();
			if (symb != nullptr) //+ проверка на наличие имени &&symb->identifier()
				switch (symb->variant())
				{
					case PROGRAM_NAME:
					case PROCEDURE_NAME:
					case FUNCTION_NAME:
					case STMTFN_STAT: {
						//cout << "this dont coppy: " << symb->identifier() << endl;
						notchange_name.insert(pair<string, SgSymbol*>{string(symb->identifier()), symb});
						break;
					}
					default:
						break;
				}
			list_check(expr_list->rhs(), notchange_name);
		}
		break;
	}
	case COMM_LIST:
	{
		break;
		/*SgObjectListExp *commonBlockList = isSgObjectListExp(work_expr);
		if (commonBlockList != nullptr) {//вариант у arr по умалчанию 550 - default
			SgExpression *body = commonBlockList->body();
			if (body != nullptr) { //+ проверка на наличие имени &&symb->identifier()
				switch (body->variant())
				{
				case EXPR_LIST:
				{
					SgExprListExp *expr_list = isSgExprListExp(body);
					CommonParce(expr_list,notchange_name);
					break;
				}
				default:
					cout << "DONT KNOW PARCE COMMON BLOCK!!!: " << endl;
					break;
				}
			}
		}*/
	}
	default:
		break;
	}
	return;
}

int parse_expr(SgExpression *work_expr, map<string, SgSymbol*>& notchange_name,int& num)//, SgFile *f, SgSymbol* name_copyfunc) {
{
	
	if (work_expr == nullptr)
		return 1;

	//work_expr->unparsestdout();

	switch (work_expr->variant())
	{
	case EXPR_LIST:
	{
		SgExprListExp *expr_list = isSgExprListExp(work_expr);
		if (expr_list != nullptr) {

			SgExprListExp *PredTmpExpr_list = nullptr;
			for (SgExprListExp *TmpExpr_list = expr_list; TmpExpr_list != nullptr; )
			{
				SgSymbol *symb = TmpExpr_list->lhs()->symbol();
				if (symb == nullptr)
					break;
				if (notchange_name.count(string(symb->identifier())))//можно доп проверку с variant()
				{
					//cout << "this delite: " << symb->identifier() << endl;

					SgExprListExp *tmpExpt = TmpExpr_list;
					tmpExpt->setLhs(nullptr);
					if (tmpExpt->rhs() == nullptr) {
						if (PredTmpExpr_list != nullptr)
							PredTmpExpr_list->setRhs(nullptr);
						return 1;
					}
					while (1)
					{
						tmpExpt->setLhs(tmpExpt->rhs()->lhs());
						if (tmpExpt->rhs()->rhs() == nullptr) {
							tmpExpt->setRhs(nullptr);
							break;
						}
						tmpExpt = tmpExpt->next();
					}
				}
				else {
					PredTmpExpr_list = TmpExpr_list;
					TmpExpr_list = TmpExpr_list->next();
				}
			}

			/*
			SgSymbol *symb = expr_list->lhs()->symbol();
			if (symb != nullptr) //+ проверка на наличие имени &&symb->identifier()
				if (notchange_name.count(string(symb->identifier())))//можно доп проверку с variant()
				{
					cout << "this delite: " << symb->identifier() << endl;

					//expr_list->lhs();//по идее необходимо пробегаться и переобразовывать лист

					SgExprListExp *tmpExpt = expr_list;
					tmpExpt->setLhs(nullptr);
					if (tmpExpt->rhs() == nullptr)
						return 1;
					while (1)
					{
						tmpExpt->setLhs(tmpExpt->rhs()->lhs());
						if (tmpExpt->rhs()->rhs() == nullptr) {
							tmpExpt->setRhs(nullptr);
							break;
						}
						tmpExpt = tmpExpt->next();
					}

					work_expr = expr_list;
					return parse_expr(work_expr, notchange_name);
				}
			return parse_expr(expr_list->rhs(), notchange_name);
			*/
		}
		break;
	}
	case COMM_LIST: 
	{
		SgSymbol *s = work_expr->symbol();
		string newName = s->identifier();
		int variant = s->variant();
		SgSymbol symb(variant, newName.c_str());
		work_expr->setSymbol(symb);
		break;
	}
	default:
		break;
	}
	return 0;
}

bool isnotdefinitionsradius(SgStatement *&stmt) {
	if (stmt->variant() == INTERFACE_STMT || stmt->variant() == INTERFACE_ASSIGNMENT || stmt->variant() == INTERFACE_OPERATOR)
	{
		SgStatement *st_end = stmt->lastNodeOfStmt(); // END INTERFACE
		stmt = st_end;
		return true;
	}
	if(isSgExecutableStatement(stmt))
		return false;
	return true;
}

void change_names_in_coppy(SgStatement* &copyoffunc, SgSymbol* start_symb, map<string, SgSymbol*>& notchange_name, int num = 0)
{
	for (SgSymbol* symb = start_symb->next(); symb != nullptr; symb = symb->next())
	{
		string s = symb->identifier();
		if (notchange_name.count(string(symb->identifier()))) //можно доп проверку с variant()
			continue;
		s += "_TG";
		s += to_string(num);
		s += "_num";
		symb->changeName(s.c_str());
	}
}

template < typename Hdrtype >
void work_with_hedr(map<string, SgSymbol*>& notchange_name, Hdrtype* Hedr)
{
	int num_param = Hedr->numberOfParameters();
	for (int i = 0; i < num_param; ++i)
	{
		SgSymbol *args = Hedr->parameter(i);
		notchange_name.insert(pair<string, SgSymbol*>{string(args->identifier()), args});
	}
}

//пробегаемся по коду-смотрим есть ли то, что не должно менять имени
//проводим изменения до основного алгоритма
//со временем будут рассматриваться все особые случае в отдельности, а для стандартных будет стандартный parce
void what_not_change(map<string, SgSymbol*>& notchange_name, SgStatement* ffunc, SgStatement* &functionConteinsStatement, int& numInterfaceFunctions,
	bool& haveReturn)
{
	for (SgStatement *tmpSt = ffunc; tmpSt != ffunc->lastNodeOfStmt(); )
	{
		tmpSt->unparsestdout();
		cout << "variant =" << tmpSt->variant() << endl << endl;
		switch (tmpSt->variant())
		{
		case PROG_HEDR:
		{
			//по сути лишняя работа
			SgProgHedrStmt *Hedr = isSgProgHedrStmt(tmpSt);
			work_with_hedr <SgProgHedrStmt>(notchange_name, Hedr);
			tmpSt = tmpSt->lexNext();
			continue;
		}
		case FUNC_HEDR:
		{
			SgFuncHedrStmt *Hedr = isSgFuncHedrStmt(tmpSt);
			work_with_hedr <SgFuncHedrStmt>(notchange_name, Hedr);
			tmpSt = tmpSt->lexNext();
			continue;
		}
		case PROC_HEDR:
		{
			SgProcHedrStmt *Hedr = isSgProcHedrStmt(tmpSt);
			work_with_hedr <SgProcHedrStmt>(notchange_name, Hedr);
			tmpSt = tmpSt->lexNext();
			continue;
		}
		
		case CONTAINS_STMT:
		{
			cout << "Warning! its variant:" << tmpSt->variant() << endl;
			cout << "dont undestend, how parse:" << tmpSt->unparse() << endl;
			functionConteinsStatement = tmpSt;
			tmpSt = tmpSt->lexNext();
			continue;
		}
		case INTERFACE_ASSIGNMENT:
		case INTERFACE_OPERATOR:
		case INTERFACE_STMT:
		{
			SgStatement *st_end = tmpSt->lastNodeOfStmt(); // END INTERFACE
			st_end = st_end->lexNext();
			while (tmpSt != st_end->lexNext())
			{
				switch (tmpSt->variant())
				{
				case PROG_HEDR:
				case FUNC_HEDR:
				case PROC_HEDR:
				{
					SgSymbol* hedrS = tmpSt->symbol();
					notchange_name.insert(pair<string, SgSymbol*>{string(hedrS->identifier()), hedrS});
					tmpSt = tmpSt->lexNext();
					++numInterfaceFunctions;
					continue;
				}
				default: {
					tmpSt = tmpSt->lexNext();
					break;
				}
				}
			}
			continue;
		}	
		case IMPL_DECL:
		case DATA_DECL:
		case SAVE_DECL:
		case VAR_DECL:
		case PARAM_DECL: 

		case FORMAT_STAT:
		case PROC_STAT:
		case COMM_STAT:
		case STMTFN_STAT:
		{
			for (int i = 0; i < 3; ++i)
			{
				if (tmpSt->expr(i) != nullptr) {
					SgExpression *expr = tmpSt->expr(i);
					list_check(expr, notchange_name);
				}
			}
			tmpSt = tmpSt->lexNext();
			continue;
		}
		//здесь не могут найтись не дублирующиеся элементы:
		case ELSEIF_NODE:
		case LOGIF_NODE:
		case CONTINUE_NODE:
		case ASSIGN_NODE:
		case GOTO_NODE:
		case ASSGOTO_NODE:
		case COMGOTO_NODE:
		case PAUSE_NODE:
		case BREAK_NODE:		
		case LOOP_NODE:
		case FOR_NODE:
		case WHERE_NODE:
		case IF_NODE:
		case CASE_NODE:
		case DO_WHILE_NODE:

		//file work
		case ENDFILE_STAT:
		case BACKSPACE_STAT:
		case WRITE_STAT:
		case READ_STAT:
		case OPEN_STAT:
		case CLOSE_STAT:
		case INQUIRE_STAT:
		case REWIND_STAT:

		case ASSIGN_STAT:
		case CONT_STAT:
		case PRINT_STAT:
		case STOP_STAT:
		case INTRIN_STAT:
		case EXTERN_STAT:
		case EQUI_STAT:

		case NAMELIST_STAT:
		case DIM_STAT:
		
		case FUNC_CALL:
		case PROC_CALL:

		case MODULE_STMT:
		case USE_STMT:
		case TARGET_STMT:
		case PUBLIC_STMT:
		case PRIVATE_STMT:	
		case CYCLE_STMT:
		case ALLOCATE_STMT:
		case DEALLOCATE_STMT:
		case NULLIFY_STMT:
		case OPTIONAL_STMT: 
		case INTENT_STMT:
		case POINTER_STMT:
		case ALLOCATABLE_STMT:
		case EXIT_STMT:

		//case BLOCK_DATA:

		case CONTROL_END:
		{
			tmpSt = tmpSt->lexNext();
			continue;
		}
		//возможно что из-за return не будет код доходить до копий
		case RETURN_STAT:
		{
			haveReturn = true;
			SgLabel newLabel(RezervLabelForReturn);// временное решение
			SgGotoStmt *ReturnGoTo = new SgGotoStmt(newLabel);
			if (tmpSt->hasLabel()) {
				SgLabel* tmpLabel = tmpSt->label();
				ReturnGoTo->setLabel(*tmpLabel);
			}
			tmpSt->insertStmtBefore(*ReturnGoTo, *tmpSt->controlParent());
			SgStatement* nextTmpSt = tmpSt->lexNext();
			tmpSt->deleteStmt();
			tmpSt = nextTmpSt;
			continue;
		}

		case ENTRY_STAT:
		//проблемы с Oпиcaниeм типa для пoльзoвaтeльcкиx имeн
		//case IMPL_DECL:
		//не ясно как правильно отработать - создавать набор или изменять 1
		//case BLOCK_DATA:
		default:
			cout << "Warning! its variant:" << tmpSt->variant() << endl;
			cout << "dont undestend, how parse:" << tmpSt->unparse() << endl;
			tmpSt = tmpSt->lexNext();
			continue;
		}
	}
	return;
}

//процесс редактирования выражений.
void change_coppy_in_start_decl(SgStatement* copyoffunc, SgFile *f, SgSymbol* name_copyfunc, map<string, SgSymbol*>& notchange_name, SgStatement* definitions_end,int& num, SgSymbol* ProgName, SgStatement* &containsStatement)
{
	for (SgStatement *copy = copyoffunc; copy != copyoffunc->lastNodeOfStmt(); )
	{	
		//cout << "variat = " << copy->variant() << endl;
		//copy->unparsestdout();
		switch (copy->variant())
		{
		//должны существовать в единственном экземпляре
		case USE_STMT:
		case IMPL_DECL:
		case MODULE_STMT:
		case STMTFN_STAT:
		case EXTERN_STAT:
		case INTRIN_STAT: {
			SgStatement* next_copy = copy->lexNext();
			copy->deleteStmt();
			copy = next_copy;
			continue;
		}
		case STRUCT_DECL:
		{
			SgStatement* next_copy = copy->lexNext();
			while (next_copy->variant() != CONTROL_END) {
				//next_copy->unparsestdout();
				next_copy = next_copy->lexNext();
			}
			next_copy = next_copy->lexNext();
			//next_copy->unparsestdout();
			copy->deleteStmt();
			copy = next_copy;
			//copy->unparsestdout();
			continue;
		}

		case CONTAINS_STMT:
		{
			cout << "Warning! its variant:" << copy->variant() << endl;
			cout << "dont undestend, how parse:" << copy->unparse() << endl;

			containsStatement = copy;
			copy = copy->lexNext();

			continue;
			/*
			//копии будут реализованы в другой итерации
			for (SgStatement* tmpCoppy = copy->lexNext(); tmpCoppy->symbol()->identifier() != ProgName->identifier();)
			{
				tmpCoppy->unparsestdout();
				tmpCoppy->deleteStmt();
				tmpCoppy = copy->lexNext();
				if (tmpCoppy->symbol() == nullptr)
					break;
				continue;
			}
			containsStatement = copy;
			return;
			*/
		}

		case ASSIGN_STAT:
		{
			SgExpression *expr = copy->expr(0);
			if (expr != nullptr) {
				SgSymbol *symb = expr->symbol();
				if (symb != nullptr)
					if (notchange_name.count(string(symb->identifier())))//можно доп проверку с variant()
					{
						copy->setExpression(1, copy->expr(0));
						copy->setExpression(2, nullptr);
					}
			}
			copy = copy->lexNext();
			continue;
		}
		//пропускаем заголовки
		case PROG_HEDR:
		case FUNC_HEDR:
		case PROC_HEDR:

		//пропускаем следующие операторы управления
		case GOTO_NODE:
		case ASSGOTO_NODE:
		case COMGOTO_NODE:
		case ASSIGN_NODE:
		case PAUSE_NODE:
		case BREAK_NODE:
		case CONTINUE_NODE:

		//пропускаем работу с файлом
		case ENDFILE_STAT:
		case BACKSPACE_STAT:
		case INQUIRE_STAT:
		case REWIND_STAT:
		case WRITE_STAT:
		case READ_STAT:
		case OPEN_STAT:
		case CLOSE_STAT:

		//не лезем в управление\вывод:
		case PRINT_STAT:
		case STOP_STAT:
		case CONT_STAT:

		//раб с пам
		case EQUI_STAT:

		//в элементы не смотрим
		case NAMELIST_STAT:
		case DIM_STAT:

		//вызовы
		case FUNC_CALL:
		case PROC_CALL:

		case EXIT_STMT:
		case CYCLE_STMT:
		case CONTROL_END:
		{
		 	        copy = copy->lexNext();
					continue;
		}
		//на будущее смотрим внутрь всех следующих управления:
		//case BLOCK_DATA:

		case LOOP_NODE:
		case FOR_NODE:
		case WHERE_NODE:
		case ELSEIF_NODE:
		case IF_NODE:
		case LOGIF_NODE:
		case CASE_NODE:
		case DO_WHILE_NODE:

		case COMM_STAT:
		case PROC_STAT:
		case FORMAT_STAT:

		//всё же решил data копировать
		case DATA_DECL:
		case SAVE_DECL:
		case VAR_DECL:
		case PARAM_DECL:

		//рассматриваем все операторы (взято на будущее - возможно внутри может что то оказаться)
		case INTENT_STMT:
		case ALLOCATABLE_STMT:
		case POINTER_STMT:
		case TARGET_STMT:
		case PUBLIC_STMT:
		case PRIVATE_STMT:
		case ALLOCATE_STMT:
		case DEALLOCATE_STMT:
		case NULLIFY_STMT:
		{
			bool emptyStatement = false;
			for (int i = 0; i < 3; ++i)
			{
				if (copy->expr(i) != nullptr) {
					SgExpression *expr = copy->expr(i);
					parse_expr(expr, notchange_name,num);
					if (i == 0) {
						SgExprListExp *expr_list = isSgExprListExp(expr);
						if(expr_list!=nullptr)
						if (expr_list->lhs() == nullptr)
						{
							SgStatement* next_copy = copy->lexNext();
							copy->deleteStmt();
							copy = next_copy;
							emptyStatement = true;
							break;
						}
					}
				}
			}
			if (emptyStatement)
				continue;
			copy = copy->lexNext();
			continue;
		}	
		case INTERFACE_ASSIGNMENT:
		case INTERFACE_OPERATOR:
		case INTERFACE_STMT:
		{
			cout << "Warning! its variant:" << copy->variant() << endl;
			cout << "INTERFACE have dont correct parse:" << copy->unparse() << endl;
			SgStatement* next_copy = copy->lexPrev();
			copy->deleteStmt();
			copy = next_copy->lexNext();
			continue;
		}

		case RETURN_STAT:
		{
			copy = copy->lexNext();
			continue;
		}
		case ENTRY_STAT:
		//проблемы с Oпиcaниeм типa для пoльзoвaтeльcкиx имeн
		//case IMPL_DECL: //решение -закинуть в 1 копию)
		//не ясно как правильно отработать - создавать набор или изменять 1
		//case BLOCK_DATA:
		default: {
			cout << "Warning! its variant:" << copy->variant() << endl;
			cout << "dont undestend, how parse:" << copy->unparse() << endl;

			copy = copy->lexNext();
			continue;
		}
		}
	}
	return;
}

void change_coppy_in_start_code(SgStatement* copyoffunc, SgFile *f, SgSymbol* name_copyfunc, map<string, SgSymbol*>& notchange_name, SgStatement* definitions_start)
{
	for (SgStatement *copy = definitions_start; copy != copyoffunc->lastNodeOfStmt(); )
	{
		//пока не расссматривал корректно реализацию всех выражений
				/*
		switch (copy->variant())
		case RETURN_STAT:
					if (copy->lexNext() == copyoffunc->lastNodeOfStmt()) {
						SgStatement* next_copy = copy->lexNext();
						copy->deleteStmt();
						copy = next_copy;
						continue;
					}*/

		for (int i = 0; i < 3; ++i)
		{
			if (copy->expr(i) != nullptr) {
				SgExpression *expr = copy->expr(i);
				//необходимо будет реализовать пробег по выполняемым операциям
				//parse_expr(expr, notchange_name, f, name_copyfunc);
			}
		}
		copy = copy->lexNext();
	}
	return;
}

void find_definitions_radius(SgStatement* function, SgStatement* &func_definitions_radius)
{
	for (SgStatement *stmt = function->lexNext(); stmt != function->lastNodeOfStmt(); stmt = stmt->lexNext()) {
		bool param = isnotdefinitionsradius(stmt);
		if (!param && stmt->variant() != CONTROL_END)
		{
			func_definitions_radius = stmt;
			return;
		}
	}
}

void insert_coppy(  SgStatement * copyoffunc,
					SgStatement * function, 
					SgStatement * copy_definitions_radius,
					SgStatement * func_definitions_radius,
					SgStatement * containsStatement,
					SgStatement * functionConteinsStatement)
{
	TransferStatementBlock(copyoffunc->lexNext(), copy_definitions_radius, func_definitions_radius);
	if(containsStatement==nullptr)
		TransferStatementBlock(copy_definitions_radius, copyoffunc->lastNodeOfStmt(), function->lastNodeOfStmt());//что делать с return
	else
		TransferStatementBlock(copy_definitions_radius, containsStatement, functionConteinsStatement);
}

SgSymbol* get_end_symbol(SgFile *f) 
{
	for (auto symb=f->firstSymbol(); symb != nullptr; symb = symb->next())
	{
		/*cout << symb->id() << " - ";
		cout << symb->identifier() << " v= ";
		cout << symb->variant();
		cout << endl;*/
		if (symb->next() == nullptr)
			return symb;
	}
	
}

void deleteDublicateFunction(SgFile *f, SgSymbol* programName)
{
	for (SgStatement* tmpCoppy = f->firstStatement()->lexNext(); tmpCoppy->symbol() != programName;)
	{
		//tmpCoppy->unparsestdout();
		tmpCoppy->deleteStmt(); 
		tmpCoppy = f->firstStatement()->lexNext();
		continue;
	}
}

void ChangeReturn(SgStatement* function, const bool& haveReturn)
{
	if (haveReturn)
	{
		SgStatement* lastNode = function->lastNodeOfStmt();
		SgStatement* returnLabel = new SgStatement(RETURN_STAT);
		SgLabel newLabel(RezervLabelForReturn);
		returnLabel->setLabel(newLabel);
		lastNode->insertStmtBefore(*returnLabel, *lastNode->controlParent());
	}
}

//создание копий в функции и размещение копий в ней
void create_coppy(SgFile *f, SgStatement* function, int &numInterfaceFunctions, int manny = MannyOfCopies)
{
	//function->unparsestdout();
	SgSymbol* ProgName = f->firstStatement()->lexNext()->symbol();

	SgStatement * func_definitions_radius = function->lexNext();
	find_definitions_radius(function, func_definitions_radius); //найти начало - конец области определения

	vector<SgSymbol*> v_end_symb;
	vector< SgStatement*> v_copyoffunc;
	vector< SgStatement*> v_copy_definitions_radius;
	vector< SgStatement*> v_containsStatement;

	SgStatement* functionConteinsStatement = nullptr;
	map<string, SgSymbol*> notchange_name;

	//предалгоритм:
	//пробегаемся чтобы найти то что не нужно изменять
	//найтти символы информация о которых должна быть до начала алгоритма
	bool haveReturn = false;
	what_not_change(notchange_name, function, functionConteinsStatement, numInterfaceFunctions, haveReturn);

	for (int i = 0; i < manny; ++i) 
	{
		//получаем последний символ
		SgSymbol* end_symb = get_end_symbol(f); // возможно есть спец функция?
		SgSymbol* fccop = &(function->symbol()->copySubprogram(*f->firstStatement()));

		if (functionConteinsStatement != nullptr) { // если менять common то тут
			deleteDublicateFunction(f, fccop);
		}
		
		SgStatement* copyoffunc = f->firstStatement()->lexNext();
		SgSymbol* hedrS = copyoffunc->symbol();
		notchange_name.insert(pair<string, SgSymbol*>{string(hedrS->identifier()), hedrS});

		//change_coppy_in_start_decl(copyoffunc,f, fccop, notchange_name, copy_definitions_radius,i); //преобразование копии которую следует вставить (часть определений)- работающая версия
		SgStatement* containsStatement = nullptr;

		change_coppy_in_start_decl(copyoffunc, f, fccop, notchange_name, function, i, ProgName, containsStatement);
		//change_coppy_in_start_code(copyoffunc, f, fccop, notchange_name, copy_definitions_radius); //преобразование копии которую следует вставить (часть выполнения)

		SgStatement * copy_definitions_radius = copyoffunc->lexNext();
		find_definitions_radius(copyoffunc, copy_definitions_radius);

		change_names_in_coppy(copyoffunc, end_symb, notchange_name, i);

		v_end_symb.push_back(end_symb);
		v_copyoffunc.push_back(copyoffunc);
		v_copy_definitions_radius.push_back(copy_definitions_radius);
		v_containsStatement.push_back(containsStatement);
	}

	for (int i = 0; i < manny; ++i) {

		SgSymbol* end_symb = v_end_symb[i];
		SgStatement* copyoffunc = v_copyoffunc[i];
		SgStatement * copy_definitions_radius = v_copy_definitions_radius[i];
		SgStatement * copyConteinsStatement = v_containsStatement[i];

		//insert_coppy(copyoffunc, function, copy_definitions_radius, func_definitions_radius
		insert_coppy(copyoffunc, function, copy_definitions_radius, func_definitions_radius, copyConteinsStatement,functionConteinsStatement);
		copyoffunc->deleteStmt();
	}

	ChangeReturn(function, haveReturn);
}

int main(int argc, char *argv[])
{
	const char *fout_name = "dvm.proj";

	SgProject proj(fout_name);
	int count_file = proj.numberOfFiles();
	for (int i = 0; i < count_file; ++i)
	{
		SgFile*f;

		f = &(proj.file(i));
		int num_routines;
		num_routines = f->numberOfFunctions();

		//запись фаил перед изменениями
		FILE *work_out = fopen("work_out.txt", "w");
		f->unparse(work_out);
		fclose(work_out);

		//проход по всем функциям
		for (int j = 0; j < num_routines; j++) {

			SgStatement*sub;
			SgSymbol*subsym;

			sub = f->functions(j);
			subsym = sub->symbol();

			printf("Function %d'sname is %s\n", i + j, subsym->identifier());
			
			int numInterfaceFunctions = 0;
			create_coppy(f, sub, numInterfaceFunctions);
			j += numInterfaceFunctions;
		}

		//после изменений
		FILE *new_out = fopen("new_out.txt", "w");
		f->unparse(new_out);
		fclose(new_out);
	}
	return 0;
}
