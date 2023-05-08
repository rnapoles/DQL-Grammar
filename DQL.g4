grammar DQL;

queryLanguage
    : (selectStatement)? EOF
    | (updateStatement)? EOF
    | (deleteStatement)? EOF
    ;

selectStatement
    : selectClause fromClause (whereClause)? (groupByClause)? (havingClause)? (orderByClause)?
    ;

updateStatement
    : updateClause (whereClause)?
    ;

deleteStatement
    : deleteClause (whereClause)?
    ;

selectClause
    : 'SELECT' ('DISTINCT')? selectExpression (',' selectExpression)*
    ;

simpleSelectClause
    : 'SELECT' ('DISTINCT')? simpleSelectExpression
    ;

updateClause
    : 'UPDATE' abstractSchemaName ('AS')? aliasIdentificationVariable 'SET' updateItem (',' updateItem)*
    ;

deleteClause
    : 'DELETE' ('FROM')? abstractSchemaName ('AS')? aliasIdentificationVariable
    ;

fromClause
    : 'FROM' identificationVariableDeclaration (',' identificationVariableDeclaration)*
    ;

subselectFromClause
    : 'FROM' subselectIdentificationVariableDeclaration (',' subselectIdentificationVariableDeclaration)*
    ;

whereClause
    : 'WHERE' conditionalExpression
    ;

havingClause
    : 'HAVING' conditionalExpression
    ;

groupByClause
    : 'GROUP' 'BY' groupByItem (',' groupByItem)*
    ;

orderByClause
    : 'ORDER' 'BY' orderByItem (',' orderByItem)*
    ;

subselect
    : simpleSelectClause subselectFromClause (whereClause)? (groupByClause)? (havingClause)? (orderByClause)?
    ;

updateItem
    : singleValuedPathExpression '=' newValue
    ;

orderByItem
    : (simpleArithmeticExpression | singleValuedPathExpression | scalarExpression | resultVariable | functionDeclaration) ('ASC' | 'DESC')?
    ;

groupByItem
    : identificationVariable
    | resultVariable
    | singleValuedPathExpression
    ;

newValue
    : simpleArithmeticExpression
    | 'NULL'
    ;

identificationVariableDeclaration
    : rangeVariableDeclaration (indexBy)? (join)*
    ;

subselectIdentificationVariableDeclaration
    : identificationVariableDeclaration
    ;

rangeVariableDeclaration
    : abstractSchemaName ('AS')? aliasIdentificationVariable
    ;

joinAssociationDeclaration
    : joinAssociationPathExpression ('AS')? aliasIdentificationVariable (indexBy)?
    ;

join
    : (('LEFT') ('OUTER')? | 'INNER')? 'JOIN' (joinAssociationDeclaration | rangeVariableDeclaration) ('WITH' conditionalExpression)?
    ;

indexBy
    : 'INDEX' 'BY' stateFieldPathExpression
    ;

selectExpression
    : (identificationVariable | singleValuedPathExpression | aggregateExpression | scalarExpression | functionDeclaration | partialObjectExpression | subSelectExpresion | caseExpression | newObjectExpression) (('AS')? ('HIDDEN')? aliasResultVariable)?
    ;
    
subSelectExpresion
    : '(' subselect ')'
    ;

simpleSelectExpression
    : (stateFieldPathExpression | identificationVariable | functionDeclaration | aggregateExpression | subSelectExpresion | scalarExpression) (('AS')? aliasResultVariable)?
    ;

partialObjectExpression
    : 'PARTIAL' identificationVariable '.' partialFieldSet
    ;

partialFieldSet
    : '{' simpleStateField (',' simpleStateField)* '}'
    ;

newObjectExpression
    : 'NEW' abstractSchemaName '(' newObjectArg (',' newObjectArg)* ')'
    ;

newObjectArg
    : scalarExpression
    | subSelectExpresion
    ;

conditionalExpression
    : conditionalTerm ('OR' conditionalTerm)*
    ;

conditionalTerm
    : conditionalFactor ('AND' conditionalFactor)*
    ;

conditionalFactor
    : ('NOT')? conditionalPrimary
    ;

conditionalPrimary
    : simpleConditionalExpression
    | '(' conditionalExpression ')'
    ;

simpleConditionalExpression
    : comparisonExpression
    | betweenExpression
    | likeExpression
    | inExpression
    | nullComparisonExpression
    | existsExpression
    | emptyCollectionComparisonExpression
    | collectionMemberExpression
    | instanceOfExpression
    ;

emptyCollectionComparisonExpression
    : collectionValuedPathExpression 'IS' ('NOT')? 'EMPTY'
    ;

collectionMemberExpression
    : entityExpression ('NOT')? 'MEMBER' ('OF')? collectionValuedPathExpression
    ;

literal
    : STRING
    | CHAR
    | INTEGER
    | FLOAT
    | BOOLEAN
    ;

inParameter
    : literal
    | inputParameter
    ;

inputParameter
    : positionalParameter
    | namedParameter
    ;

positionalParameter
    : '?' INTEGER
    ;

namedParameter
    : ':' identificationVariable
    ;

arithmeticExpression
    : simpleArithmeticExpression | subSelectExpresion
    ;

simpleArithmeticExpression
    : arithmeticTerm (('+' | '-') arithmeticTerm)*
    ;

arithmeticTerm
    : arithmeticFactor (('*' | '/') arithmeticFactor)*
    ;

arithmeticFactor
    : ('+' | '-')? arithmeticPrimary
    ;

arithmeticPrimary
    : singleValuedPathExpression
    | literal
    | '(' simpleArithmeticExpression ')'
    | functionsReturningNumerics
    | aggregateExpression
    | functionsReturningStrings
    | functionsReturningDatetime
    | identificationVariable
    | resultVariable
    | inputParameter
    | caseExpression
    ;

scalarExpression
    : simpleArithmeticExpression
    | stringPrimary
    | datetimePrimary
    | stateFieldPathExpression
    | booleanPrimary
    | caseExpression
    | instanceOfExpression
    ;

stringExpression
    : stringPrimary
    | resultVariable
    | subSelectExpresion
    ;

stringPrimary
    : stateFieldPathExpression
    | STRING
    | inputParameter
    | functionsReturningStrings
    | aggregateExpression
    | caseExpression
    ;

booleanExpression
    : booleanPrimary
    | subSelectExpresion
    ;

booleanPrimary
    : stateFieldPathExpression
    | BOOLEAN
    | inputParameter
    ;

entityExpression
    : singleValuedAssociationPathExpression
    | simpleEntityExpression
    ;

simpleEntityExpression
    : identificationVariable
    | inputParameter
    ;

datetimeExpression
    : datetimePrimary
    | subSelectExpresion
    ;

datetimePrimary
    : stateFieldPathExpression
    | inputParameter
    | functionsReturningDatetime
    | aggregateExpression
    ;

aggregateExpression
    : ('AVG' | 'MAX' | 'MIN' | 'SUM' | 'COUNT') '(' ('DISTINCT')? simpleArithmeticExpression ')'
    ;

caseExpression
    : generalCaseExpression
    | simpleCaseExpression
    | coalesceExpression
    | nullIfExpression
    ;

generalCaseExpression
    : 'CASE' whenClause (whenClause)* 'ELSE' scalarExpression 'END'
    ;

whenClause
    : 'WHEN' conditionalExpression 'THEN' scalarExpression
    ;

simpleCaseExpression
    : 'CASE' caseOperand simpleWhenClause (simpleWhenClause)* 'ELSE' scalarExpression 'END'
    ;

caseOperand
    : stateFieldPathExpression
    //| TypeDiscriminator
    ;

simpleWhenClause
    : 'WHEN' scalarExpression 'THEN' scalarExpression
    ;

coalesceExpression
    : 'COALESCE' '(' scalarExpression (',' scalarExpression)* ')'
    ;

nullIfExpression
    : 'NULLIF' '(' scalarExpression ',' scalarExpression ')'
    ;

quantifiedExpression
    : ('ALL' | 'ANY' | 'SOME') subSelectExpresion
    ;

betweenExpression
    : arithmeticExpression ('NOT')? 'BETWEEN' arithmeticExpression 'AND' arithmeticExpression
    ;

comparisonExpression
    : arithmeticExpression comparisonOperator ( quantifiedExpression | arithmeticExpression )
    ;

inExpression
    : singleValuedPathExpression ('NOT')? 'IN' '(' (inParameter (',' inParameter)* | subselect) ')'
    ;

instanceOfExpression
    : identificationVariable ('NOT')? 'INSTANCE' ('OF')? (instanceOfParameter | '(' instanceOfParameter (',' instanceOfParameter)* ')')
    ;

instanceOfParameter
    : abstractSchemaName | inputParameter
    ;

likeExpression
    : stringExpression ('NOT')? 'LIKE' stringPrimary ('ESCAPE' CHAR)?
    ;

nullComparisonExpression
    : (inputParameter | nullIfExpression | coalesceExpression | aggregateExpression | functionDeclaration | identificationVariable | singleValuedPathExpression | resultVariable) 'IS' ('NOT')? 'NULL'
    ;

existsExpression
    : ('NOT')? 'EXISTS' subSelectExpresion
    ;

comparisonOperator
    : '=' 
    | '<' 
    | '<=' 
    | '<>' 
    | '>' 
    | '>=' 
    | '!='
    ;

functionDeclaration
    : functionsReturningStrings
    | functionsReturningNumerics
    | functionsReturningDatetime
    ;

functionsReturningNumerics
    : 'LENGTH' '(' stringPrimary ')'
    | 'LOCATE' '(' stringPrimary ',' stringPrimary (',' simpleArithmeticExpression)?')'
    | 'ABS' '(' simpleArithmeticExpression ')'
    | 'SQRT' '(' simpleArithmeticExpression ')'
    | 'MOD' '(' simpleArithmeticExpression ',' simpleArithmeticExpression ')'
    | 'SIZE' '(' collectionValuedPathExpression ')'
    | 'DATE_DIFF' '(' arithmeticPrimary ',' arithmeticPrimary ')'
    | 'BIT_AND' '(' arithmeticPrimary ',' arithmeticPrimary ')'
    | 'BIT_OR' '(' arithmeticPrimary ',' arithmeticPrimary ')'
    ;

functionsReturningDatetime
    : 'CURRENT_DATE'
    | 'CURRENT_TIME'
    | 'CURRENT_TIMESTAMP'
    | 'DATE_ADD' '(' arithmeticPrimary ',' arithmeticPrimary ',' stringPrimary ')'
    | 'DATE_SUB' '(' arithmeticPrimary ',' arithmeticPrimary ',' stringPrimary ')'
    ;

functionsReturningStrings
    : 'CONCAT' '(' stringPrimary ',' stringPrimary ')'
    | 'SUBSTRING' '(' stringPrimary ',' simpleArithmeticExpression ',' simpleArithmeticExpression ')'
    | 'TRIM' '(' (('LEADING' | 'TRAILING' | 'BOTH')? (CHAR)? 'FROM')? stringPrimary ')'
    | 'LOWER' '(' stringPrimary ')'
    | 'UPPER' '(' stringPrimary ')'
    | 'IDENTITY' '(' singleValuedAssociationPathExpression (',' STRING)* ')'
    ;

/* 'u.Group' or 'u.Phonenumbers' declarations */
joinAssociationPathExpression
    : identificationVariable '.' (collectionValuedAssociationField | singleValuedAssociationField)
    ;

/* 'u.Group' or 'u.Phonenumbers' usages */
associationPathExpression
    : collectionValuedPathExpression
    | singleValuedAssociationPathExpression
    ;

/* 'u.name' or 'u.Group' */
singleValuedPathExpression
    : stateFieldPathExpression
    | singleValuedAssociationPathExpression
    ;

/* 'u.name' or 'u.Group.name' */
stateFieldPathExpression
    : identificationVariable '.' stateField
    ;

/* 'u.Group' */
singleValuedAssociationPathExpression
    : identificationVariable '.' singleValuedAssociationField
    ;

/* 'u.Group.Permissions' */
collectionValuedPathExpression
    : identificationVariable '.' collectionValuedAssociationField
    ;

/* 'name' */
stateField
    : (embeddedClassStateField '.')* simpleStateField
    ;

/* Alias Identification usage (the "u" of "u.name") */
identificationVariable
    : identifier
    ;

/* Alias Identification declaration (the "u" of "FROM User u") */
aliasIdentificationVariable
    : identifier
    ;

/* identifier that must be a class name (the "User" of "FROM User u"), possibly as a fully qualified class name or namespace-aliased */
abstractSchemaName
    : FULLY_QUALIFIED_NAME
    //| aliased_name //CMS:CmsUser
    | identifier
    ;

/* Alias resultVariable declaration (the "total" of "COUNT(*) AS total") */
aliasResultVariable
    : identifier
    ;

/* resultVariable identifier usage of mapped field aliases (the "total" of "COUNT(*) AS total") */
resultVariable
    : identifier
    ;

/* identifier that must be a field (the "name" of "u.name") */
/* This is responsible to know if the field exists in Object, no matter if it's a relation or a simple field */
fieldIdentificationVariable
    : identifier
    ;

/* identifier that must be a collection-valued association field (to-many) (the "Phonenumbers" of "u.Phonenumbers") */
collectionValuedAssociationField
    : fieldIdentificationVariable
    ;

/* identifier that must be a single-valued association field (to-one) (the "Group" of "u.Group") */
singleValuedAssociationField
    : fieldIdentificationVariable
    ;

/* identifier that must be an embedded class state field */
embeddedClassStateField
    : fieldIdentificationVariable
    ;

/* identifier that must be a simple state field (name, email, ...) (the "name" of "u.name") */
/* The difference between this and fieldIdentificationVariable is only semantical, because it points to a single field (not mapping to a relation) */
simpleStateField
    : fieldIdentificationVariable
    ;

identifier
    : IDENTIFIER
    | 'ALL'
    | 'AND'
    | 'ANY'
    | 'AS'
    | 'ASC'
    | 'AVG'
    | 'BETWEEN'
    | 'BOTH'
    | 'BY'
    | 'CASE'
    | 'COALESCE'
    | 'COUNT'
    | 'DELETE'
    | 'DESC'
    | 'DISTINCT'
    | 'ELSE'
    | 'EMPTY'
    | 'END'
    | 'ESCAPE'
    | 'EXISTS'
    | 'FALSE'
    | 'FROM'
    | 'GROUP'
    | 'HAVING'
    | 'HIDDEN'
    | 'IN'
    | 'INDEX'
    | 'INNER'
    | 'INSTANCE'
    | 'IS'
    | 'JOIN'
    | 'LEADING'
    | 'LEFT'
    | 'LIKE'
    | 'MAX'
    | 'MEMBER'
    | 'MIN'
    | 'NEW'
    | 'NOT'
    | 'NULL'
    | 'NULLIF'
    | 'OF'
    | 'OR'
    | 'ORDER'
    | 'OUTER'
    | 'PARTIAL'
    | 'SELECT'
    | 'SET'
    | 'SOME'
    | 'SUM'
    | 'THEN'
    | 'TRAILING'
    | 'TRUE'
    | 'UPDATE'
    | 'WHEN'
    | 'WHERE'
    | 'WITH'
;

IDENTIFIER
    : [a-zA-Z_][a-zA-Z_0-9]* 
;

FULLY_QUALIFIED_NAME 
    : [A-Z][a-zA-Z0-9]* //Doctrine\Tests\Models\CMS\CmsUser
    ;
CHAR
   : '\'' (~ ('\'' | '\\')) '\''
   ;

STRING
   : ('\'' (~ ('\\' | '"'))* '\'')
   ;

FLOAT
 : DIGIT+ ( '.' DIGIT* )? ( 'E' [-+]? DIGIT+ )?
 | '.' DIGIT+ ( 'E' [-+]? DIGIT+ )?
 ;

INTEGER
    : DIGIT+
    ;

BOOLEAN
   : 'true'
   | 'false'
   ;

WS
   : [ \t\r\n] -> skip
   ;

fragment DIGIT : [0-9];

