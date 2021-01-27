import { CstNode, IToken } from 'chevrotain';

import { Brace, Curly, MultiIdentifier, Opt, OptCommas } from './helpers';

type OptAnnotations = Opt<{ annotation: AnnotationCstNode[] }>;

//Arrays

export interface ArrayInitializerCstNode extends CstNode {
  name: 'arrayInitializer';
  children: ArrayInitializerCtx;
}

export type ArrayInitializerCtx = Opt<{
  variableInitializerList: [VariableInitializerListCstNode];
}> &
  Opt<{
    Comma: [IToken];
  }> &
  Curly;

export interface VariableInitializerListCstNode extends CstNode {
  name: 'variableInitializerList';
  children: VariableInitializerListCtx;
}

export type VariableInitializerListCtx = {
  variableInitializer: [VariableInitializerCstNode];
} & Opt<{
  Comma: IToken[];
  variableInitializer: VariableInitializerCstNode[];
}>;

//Blocks and statements

export interface BlockCstNode extends CstNode {
  name: 'block';
  children: BlockCtx;
}

export type BlockCtx = Opt<{
  blockStatements: BlockStatementsCstNode[];
}> &
  Curly;

export interface BlockStatementsCstNode extends CstNode {
  name: 'blockStatements';
  children: BlockStatementsCtx;
}

export type BlockStatementsCtx = {
  blockStatement: [BlockStatementCstNode] & BlockStatementCstNode[];
};

export interface BlockStatementCstNode extends CstNode {
  name: 'blockStatement';
  children: BlockStatementCtx;
}

export type BlockStatementCtx =
  | {
      localVariableDeclarationStatement: [
        LocalVariableDeclarationStatementCstNode
      ];
    }
  | {
      classDeclaration: [ClassDeclarationCstNode];
    }
  | {
      statement: [StatementCstNode];
    };

export interface LocalVariableDeclarationStatementCstNode extends CstNode {
  name: 'localVariableDeclarationStatement';
  children: LocalVariableDeclarationStatementCtx;
}

export type LocalVariableDeclarationStatementCtx = {
  localVariableDeclaration: [LocalVariableDeclarationCstNode];
  Semicolon: [IToken];
};

export interface LocalVariableDeclarationCstNode extends CstNode {
  name: 'localVariableDeclaration';
  children: LocalVariableDeclarationCtx;
}

export type LocalVariableDeclarationCtx = {
  localVariableType: [LocalVariableTypeCstNode];
  variableDeclaratorList: [VariableDeclaratorListCstNode];
} & Opt<{
  variableModifier: VariableModifierCstNode[];
}>;

export interface LocalVariableTypeCstNode extends CstNode {
  name: 'localVariableType';
  children: LocalVariableTypeCtx;
}

export type LocalVariableTypeCtx =
  | {
      unannType: [UnannTypeCstNode];
    }
  | {
      Var: [IToken];
    };

export interface StatementCstNode extends CstNode {
  name: 'statement';
  children: StatementCtx;
}

export type StatementCtx =
  | {
      statementWithoutTrailingSubstatement: [
        StatementWithoutTrailingSubstatementCstNode
      ];
    }
  | {
      labeledStatement: [LabeledStatementCstNode];
    }
  | {
      ifStatement: [IfStatementCstNode];
    }
  | {
      whileStatement: [WhileStatementCstNode];
    }
  | {
      forStatement: [ForStatementCstNode];
    };

export interface StatementWithoutTrailingSubstatementCstNode extends CstNode {
  name: 'statementWithoutTrailingSubstatement';
  children: StatementWithoutTrailingSubstatementCtx;
}

export type StatementWithoutTrailingSubstatementCtx =
  | {
      block: [BlockCstNode];
    }
  | {
      yieldStatement: [YieldStatementCstNode];
    }
  | {
      emptyStatement: [EmptyStatementCstNode];
    }
  | {
      expressionStatement: [ExpressionStatementCstNode];
    }
  | {
      assertStatement: [AssertStatementCstNode];
    }
  | {
      switchStatement: [SwitchStatementCstNode];
    }
  | {
      doStatement: [DoStatementCstNode];
    }
  | {
      breakStatement: [BreakStatementCstNode];
    }
  | {
      continueStatement: [ContinueStatementCstNode];
    }
  | {
      returnStatement: [ReturnStatementCstNode];
    }
  | {
      synchronizedStatement: [SynchronizedStatementCstNode];
    }
  | {
      throwStatement: [ThrowStatementCstNode];
    }
  | {
      tryStatement: [TryStatementCstNode];
    };

export interface EmptyStatementCstNode extends CstNode {
  name: 'emptyStatement';
  children: EmptyStatementCtx;
}

export type EmptyStatementCtx = {
  Semicolon: [IToken];
};

export interface LabeledStatementCstNode extends CstNode {
  name: 'labeledStatement';
  children: LabeledStatementCtx;
}

export type LabeledStatementCtx = {
  Identifier: [IToken];
  Colon: [IToken];
  statement: [StatementCstNode];
};

export interface ExpressionStatementCstNode extends CstNode {
  name: 'expressionStatement';
  children: ExpressionStatementCtx;
}

export type ExpressionStatementCtx = {
  statementExpression: [StatementExpressionCstNode];
  Semicolon: [IToken];
};

export interface StatementExpressionCstNode extends CstNode {
  name: 'statementExpression';
  children: StatementExpressionCtx;
}

export type StatementExpressionCtx = {
  expression: [ExpressionCstNode];
};

export interface IfStatementCstNode extends CstNode {
  name: 'ifStatement';
  children: IfStatementCtx;
}

export type IfStatementCtx = {
  If: [IToken];
  expression: [ExpressionCstNode];
} & (
  | {
      statement: [StatementCstNode];
    }
  | {
      statement: [StatementCstNode, StatementCstNode];
      Else: [IToken];
    }
) &
  Brace;

export interface AssertStatementCstNode extends CstNode {
  name: 'assertStatement';
  children: AssertStatementCtx;
}

export type AssertStatementCtx = {
  Assert: [IToken];
  Semicolon: [IToken];
} & (
  | {
      expression: [ExpressionCstNode, ExpressionCstNode];
      Colon: [IToken];
    }
  | {
      expression: [ExpressionCstNode];
    }
);

export interface SwitchStatementCstNode extends CstNode {
  name: 'switchStatement';
  children: SwitchStatementCtx;
}

export type SwitchStatementCtx = {
  Switch: [IToken];
  expression: [ExpressionCstNode];
  switchBlock: [SwitchBlockCstNode];
} & Brace;

export interface SwitchBlockCstNode extends CstNode {
  name: 'switchBlock';
  children: SwitchBlockCtx;
}

export type SwitchBlockCtx = Curly &
  (
    | {
        switchBlockStatementGroup: SwitchBlockStatementGroupCstNode[];
      }
    | {
        switchRule: SwitchRuleCstNode[];
      }
  );

export interface SwitchBlockStatementGroupCstNode extends CstNode {
  name: 'switchBlockStatementGroup';
  children: SwitchBlockStatementGroupCtx;
}

export type SwitchBlockStatementGroupCtx = {
  switchLabel: SwitchLabelCstNode[];
  Colon: IToken[];
} & Opt<{
  blockStatements: [BlockStatementsCstNode];
}>;

export interface SwitchLabelCstNode extends CstNode {
  name: 'switchLabel';
  children: SwitchLabelCtx;
}

export type SwitchLabelCtx =
  | ({
      Case: [IToken];
      caseConstant: [CaseConstantCstNode];
    } & Opt<{
      Comma: IToken[];
      caseConstant: CaseConstantCstNode[];
    }>)
  | {
      Default: [IToken];
    };

export interface SwitchRuleCstNode extends CstNode {
  name: 'switchRule';
  children: SwitchRuleCtx;
}

export type SwitchRuleCtx = {
  switchLabel: [SwitchLabelCstNode];
  Arrow: [IToken];
} & (
  | {
      throwStatement: [ThrowStatementCstNode];
    }
  | {
      block: [BlockCstNode];
    }
  | {
      expression: [ExpressionCstNode];
      Semicolon: [IToken];
    }
);

export interface CaseConstantCstNode extends CstNode {
  name: 'caseConstant';
  children: CaseConstantCtx;
}

export type CaseConstantCtx = {
  ternaryExpression: [TernaryExpressionCstNode];
};

export interface WhileStatementCstNode extends CstNode {
  name: 'whileStatement';
  children: WhileStatementCtx;
}

export type WhileStatementCtx = {
  While: [IToken];
  expression: [ExpressionCstNode];
  statement: [StatementCstNode];
} & Brace;

export interface DoStatementCstNode extends CstNode {
  name: 'doStatement';
  children: DoStatementCtx;
}

export type DoStatementCtx = {
  Do: [IToken];
  statement: [StatementCstNode];
  While: [IToken];
  expression: [ExpressionCstNode];
  Semicolon: [IToken];
} & Brace;

export interface ForStatementCstNode extends CstNode {
  name: 'forStatement';
  children: ForStatementCtx;
}

export type ForStatementCtx =
  | {
      basicForStatement: [BasicForStatementCstNode];
    }
  | {
      enhancedForStatement: [EnhancedForStatementCstNode];
    };

export interface BasicForStatementCstNode extends CstNode {
  name: 'basicForStatement';
  children: BasicForStatementCtx;
}

export type BasicForStatementCtx = {
  For: [IToken];
  Semicolon: [IToken];
  statement: [StatementCstNode];
} & Opt<{
  forInit: [ForInitCstNode];
}> &
  Opt<{
    expression: [ExpressionCstNode];
  }> &
  Opt<{
    forUpdate: [ForUpdateCstNode];
  }> &
  Brace;

export interface ForInitCstNode extends CstNode {
  name: 'forInit';
  children: ForInitCtx;
}

export type ForInitCtx =
  | {
      localVariableDeclaration: [LocalVariableDeclarationCstNode];
    }
  | {
      statementExpressionList: [StatementExpressionListCstNode];
    };

export interface ForUpdateCstNode extends CstNode {
  name: 'forUpdate';
  children: ForUpdateCtx;
}

export type ForUpdateCtx = {
  statementExpressionList: [StatementExpressionListCstNode];
};

export interface StatementExpressionListCstNode extends CstNode {
  name: 'statementExpressionList';
  children: StatementExpressionListCtx;
}

export type StatementExpressionListCtx = {
  statementExpression: [StatementExpressionCstNode];
} & Opt<{
  statementExpression: StatementExpressionCstNode[];
  Comma: IToken[];
}>;

export interface EnhancedForStatementCstNode extends CstNode {
  name: 'enhancedForStatement';
  children: EnhancedForStatementCtx;
}

export type EnhancedForStatementCtx = {
  For: [IToken];
  localVariableType: [LocalVariableTypeCstNode];
  variableDeclaratorId: [VariableDeclaratorIdCstNode];
  Colon: [IToken];
  expression: [ExpressionCstNode];
  statement: [StatementCstNode];
} & Opt<{
  variableModifier: VariableModifierCstNode[];
}> &
  Brace;

export interface BreakStatementCstNode extends CstNode {
  name: 'breakStatement';
  children: BreakStatementCtx;
}

export type BreakStatementCtx = {
  Break: [IToken];
  Semicolon: [IToken];
} & Opt<{
  Identifier: [IToken];
}>;

export interface ContinueStatementCstNode extends CstNode {
  name: 'continueStatement';
  children: ContinueStatementCtx;
}

export type ContinueStatementCtx = {
  Continue: [IToken];
  Semicolon: [IToken];
} & Opt<{
  Identifier: [IToken];
}>;

export interface ReturnStatementCstNode extends CstNode {
  name: 'returnStatement';
  children: ReturnStatementCtx;
}

export type ReturnStatementCtx = {
  Return: [IToken];
  Semicolon: [IToken];
} & Opt<{
  expression: [ExpressionCstNode];
}>;

export interface ThrowStatementCstNode extends CstNode {
  name: 'throwStatement';
  children: ThrowStatementCtx;
}

export type ThrowStatementCtx = {
  Throw: [IToken];
  expression: [ExpressionCstNode];
  Semicolon: [IToken];
};

export interface SynchronizedStatementCstNode extends CstNode {
  name: 'synchronizedStatement';
  children: SynchronizedStatementCtx;
}

export type SynchronizedStatementCtx = {
  Synchronized: [IToken];
  expression: [ExpressionCstNode];
  block: [BlockCstNode];
} & Brace;

export interface TryStatementCstNode extends CstNode {
  name: 'tryStatement';
  children: TryStatementCtx;
}

export type TryStatementCtx =
  | ({
      Try: [IToken];
      block: [BlockCstNode];
    } & (
      | ({
          catches: [CatchesCstNode];
        } & Opt<{
          finally: [FinallyCstNode];
        }>)
      | {
          finally: [FinallyCstNode];
        }
    ))
  | {
      tryWithResourcesStatement: [TryWithResourcesStatementCstNode];
    };

export interface CatchesCstNode extends CstNode {
  name: 'catches';
  children: CatchesCtx;
}

export type CatchesCtx = {
  catchClause: [CatchClauseCstNode] & CatchClauseCstNode[];
};

export interface CatchClauseCstNode extends CstNode {
  name: 'catchClause';
  children: CatchClauseCtx;
}

export type CatchClauseCtx = {
  Catch: [IToken];
  catchFormalParameter: [CatchFormalParameterCstNode];
  block: [BlockCstNode];
} & Brace;

export interface CatchFormalParameterCstNode extends CstNode {
  name: 'catchFormalParameter';
  children: CatchFormalParameterCtx;
}

export type CatchFormalParameterCtx = {
  catchType: [CatchTypeCstNode];
  variableDeclaratorId: [VariableDeclaratorIdCstNode];
} & Opt<{
  variableModifier: VariableModifierCstNode[];
}>;

export interface CatchTypeCstNode extends CstNode {
  name: 'catchType';
  children: CatchTypeCtx;
}

export type CatchTypeCtx = {
  unannClassType: [UnannClassTypeCstNode];
} & Opt<{
  Or: IToken[];
  classType: ClassTypeCstNode[];
}>;

export interface FinallyCstNode extends CstNode {
  name: 'finally';
  children: FinallyCtx;
}

export type FinallyCtx = {
  Finally: [IToken];
  block: [BlockCstNode];
};

export interface TryWithResourcesStatementCstNode extends CstNode {
  name: 'tryWithResourcesStatement';
  children: TryWithResourcesStatementCtx;
}

export type TryWithResourcesStatementCtx = {
  Try: [IToken];
  resourceSpecification: [ResourceSpecificationCstNode];
  block: [BlockCstNode];
} & Opt<{
  catches: [CatchesCstNode];
}> &
  Opt<{
    finally: [FinallyCstNode];
  }>;

export interface ResourceSpecificationCstNode extends CstNode {
  name: 'resourceSpecification';
  children: ResourceSpecificationCtx;
}

export type ResourceSpecificationCtx = {
  resourceList: [ResourceListCstNode];
} & Opt<{
  Semicolon: [IToken];
}> &
  Brace;

export interface ResourceListCstNode extends CstNode {
  name: 'resourceList';
  children: ResourceListCtx;
}

export type ResourceListCtx = {
  resource: [ResourceCstNode];
} & Opt<{
  resource: ResourceCstNode[];
  Semicolon: IToken[];
}>;

export interface ResourceCstNode extends CstNode {
  name: 'resource';
  children: ResourceCtx;
}

export type ResourceCtx =
  | {
      resourceInit: [ResourceInitCstNode];
    }
  | {
      variableAccess: [VariableAccessCstNode];
    };

export interface ResourceInitCstNode extends CstNode {
  name: 'resourceInit';
  children: ResourceInitCtx;
}

export type ResourceInitCtx = {
  localVariableType: [LocalVariableTypeCstNode];
  Identifier: [IToken];
  Equals: [IToken];
  expression: [ExpressionCstNode];
} & Opt<{
  variableModifier: VariableModifierCstNode[];
}>;

export interface YieldStatementCstNode extends CstNode {
  name: 'yieldStatement';
  children: YieldStatementCtx;
}

export type YieldStatementCtx = {
  Yield: [IToken];
  expression: [ExpressionCstNode];
  Semicolon: [IToken];
};

export interface VariableAccessCstNode extends CstNode {
  name: 'variableAccess';
  children: VariableAccessCtx;
}

export type VariableAccessCtx = {
  primary: [PrimaryCstNode];
};

export interface IsBasicForStatementCstNode extends CstNode {
  name: 'isBasicForStatement';
  children: IsBasicForStatementCtx;
}

export type IsBasicForStatementCtx = {
  For: IToken[];
  LBrace: IToken[];
  Semicolon: IToken[];
} & Opt<{
  forInit: ForInitCstNode[];
}>;

export interface IsLocalVariableDeclarationCstNode extends CstNode {
  name: 'isLocalVariableDeclaration';
  children: IsLocalVariableDeclarationCtx;
}

export type IsLocalVariableDeclarationCtx = {
  localVariableType: [LocalVariableTypeCstNode];
  variableDeclaratorId: [VariableDeclaratorIdCstNode];
} & Opt<{
  variableModifier: VariableModifierCstNode[];
}>;

export interface IsClassicSwitchLabelCstNode extends CstNode {
  name: 'isClassicSwitchLabel';
  children: IsClassicSwitchLabelCtx;
}

export type IsClassicSwitchLabelCtx = {
  switchLabel: [SwitchLabelCstNode];
  Colon: [IToken];
};

//Classes

export interface ClassDeclarationCstNode extends CstNode {
  name: 'classDeclaration';
  children: ClassDeclarationCtx;
}

export type ClassDeclarationCtx = (
  | {
      normalClassDeclaration: NormalClassDeclarationCstNode[];
    }
  | {
      enumDeclaration: EnumDeclarationCstNode[];
    }
) &
  Opt<{
    classModifier: ClassModifierCstNode[];
  }>;

export interface NormalClassDeclarationCstNode extends CstNode {
  name: 'normalClassDeclaration';
  children: NormalClassDeclarationCtx;
}

export type NormalClassDeclarationCtx = {
  Class: [IToken];
  typeIdentifier: [TypeIdentifierCstNode];
  classBody: [ClassBodyCstNode];
} & Opt<{
  typeParameters: [TypeParametersCstNode];
}> &
  Opt<{
    superclass: [SuperclassCstNode];
  }> &
  Opt<{
    superinterfaces: [SuperinterfacesCstNode];
  }>;

export interface ClassModifierCstNode extends CstNode {
  name: 'classModifier';
  children: ClassModifierCtx;
}

export type ClassModifierCtx = {
  annotation: [AnnotationCstNode];
  Public: [IToken];
  Protected: [IToken];
  Private: [IToken];
  Abstract: [IToken];
  Static: [IToken];
  Final: [IToken];
  Strictfp: [IToken];
};

export interface TypeParametersCstNode extends CstNode {
  name: 'typeParameters';
  children: TypeParametersCtx;
}

export type TypeParametersCtx = {
  Less: [IToken];
  typeParameterList: [TypeParameterListCstNode];
  Greater: [IToken];
};

export interface TypeParameterListCstNode extends CstNode {
  name: 'typeParameterList';
  children: TypeParameterListCtx;
}

export type TypeParameterListCtx = {
  typeParameter: TypeParameterCstNode[];
} & OptCommas;

export interface SuperclassCstNode extends CstNode {
  name: 'superclass';
  children: SuperclassCtx;
}

export type SuperclassCtx = {
  Extends: [IToken];
  classType: [ClassTypeCstNode];
};

export interface SuperinterfacesCstNode extends CstNode {
  name: 'superinterfaces';
  children: SuperinterfacesCtx;
}

export type SuperinterfacesCtx = {
  Implements: [IToken];
  interfaceTypeList: [InterfaceTypeListCstNode];
};

export interface InterfaceTypeListCstNode extends CstNode {
  name: 'interfaceTypeList';
  children: InterfaceTypeListCtx;
}

export type InterfaceTypeListCtx = {
  interfaceType: InterfaceTypeCstNode[];
} & OptCommas;

export interface ClassBodyCstNode extends CstNode {
  name: 'classBody';
  children: ClassBodyCtx;
}

export type ClassBodyCtx = Curly &
  Opt<{
    classBodyDeclaration: ClassBodyDeclarationCstNode[];
  }>;

export interface ClassBodyDeclarationCstNode extends CstNode {
  name: 'classBodyDeclaration';
  children: ClassBodyDeclarationCtx;
}

export type ClassBodyDeclarationCtx =
  | {
      classMemberDeclaration: [ClassMemberDeclarationCstNode];
    }
  | {
      instanceInitializer: [InstanceInitializerCstNode];
    }
  | {
      staticInitializer: [StaticInitializerCstNode];
    }
  | {
      constructorDeclaration: [ConstructorDeclarationCstNode];
    };

export interface ClassMemberDeclarationCstNode extends CstNode {
  name: 'classMemberDeclaration';
  children: ClassMemberDeclarationCtx;
}

export type ClassMemberDeclarationCtx =
  | {
      fieldDeclaration: [FieldDeclarationCstNode];
    }
  | {
      methodDeclaration: [MethodDeclarationCstNode];
    }
  | {
      classDeclaration: [ClassDeclarationCstNode];
    }
  | {
      interfaceDeclaration: [InterfaceDeclarationCstNode];
    }
  | {
      Semicolon: [IToken];
    };

export interface FieldDeclarationCstNode extends CstNode {
  name: 'fieldDeclaration';
  children: FieldDeclarationCtx;
}

export type OptFieldModifiers = Opt<{
  fieldModifier: FieldModifierCstNode[];
}>;

export type FieldDeclarationCtx = {
  unannType: [UnannTypeCstNode];
  variableDeclaratorList: [VariableDeclaratorListCstNode];
  Semicolon: [IToken];
} & OptFieldModifiers;

export interface FieldModifierCstNode extends CstNode {
  name: 'fieldModifier';
  children: FieldModifierCtx;
}

export type FieldModifierCtx =
  | {
      annotation: [AnnotationCstNode];
    }
  | {
      Public: [IToken];
    }
  | {
      Protected: [IToken];
    }
  | {
      Private: [IToken];
    }
  | {
      Static: [IToken];
    }
  | {
      Final: [IToken];
    }
  | {
      Transient: [IToken];
    }
  | {
      Volatile: [IToken];
    };

export interface VariableDeclaratorListCstNode extends CstNode {
  name: 'variableDeclaratorList';
  children: VariableDeclaratorListCtx;
}

export type VariableDeclaratorListCtx = {
  variableDeclarator: VariableDeclaratorCstNode[];
} & OptCommas;

export interface VariableDeclaratorCstNode extends CstNode {
  name: 'variableDeclarator';
  children: VariableDeclaratorCtx;
}

export type VariableDeclaratorCtx = {
  variableDeclaratorId: [VariableDeclaratorIdCstNode];
} & Opt<{
  Equals: [IToken];
  variableInitializer: [VariableInitializerCstNode];
}>;

export interface VariableDeclaratorIdCstNode extends CstNode {
  name: 'variableDeclaratorId';
  children: VariableDeclaratorIdCtx;
}

export type VariableDeclaratorIdCtx = {
  Identifier: [IToken];
} & Opt<{
  dims: [DimsCstNode];
}>;

export interface VariableInitializerCstNode extends CstNode {
  name: 'variableInitializer';
  children: VariableInitializerCtx;
}

export type VariableInitializerCtx =
  | {
      expression: [ExpressionCstNode];
    }
  | {
      arrayInitializer: [ArrayInitializerCstNode];
    };

export interface UnannTypeCstNode extends CstNode {
  name: 'unannType';
  children: UnannTypeCtx;
}

export type UnannTypeCtx =
  | {
      unannPrimitiveTypeWithOptionalDimsSuffix: [
        UnannPrimitiveTypeWithOptionalDimsSuffixCstNode
      ];
    }
  | {
      unannReferenceType: [UnannReferenceTypeCstNode];
    };

export interface UnannPrimitiveTypeWithOptionalDimsSuffixCstNode
  extends CstNode {
  name: 'unannPrimitiveTypeWithOptionalDimsSuffix';
  children: UnannPrimitiveTypeWithOptionalDimsSuffixCtx;
}

export type UnannPrimitiveTypeWithOptionalDimsSuffixCtx = {
  unannPrimitiveType: [UnannPrimitiveTypeCstNode];
} & Opt<{
  dims: [DimsCstNode];
}>;

export interface UnannPrimitiveTypeCstNode extends CstNode {
  name: 'unannPrimitiveType';
  children: UnannPrimitiveTypeCtx;
}

export type UnannPrimitiveTypeCtx =
  | {
      numericType: [NumericTypeCstNode];
    }
  | {
      Boolean: [IToken];
    };

export interface UnannReferenceTypeCstNode extends CstNode {
  name: 'unannReferenceType';
  children: UnannReferenceTypeCtx;
}

export type UnannReferenceTypeCtx = {
  unannClassOrInterfaceType: [UnannClassOrInterfaceTypeCstNode];
} & Opt<{
  dims: [DimsCstNode];
}>;

export interface UnannClassOrInterfaceTypeCstNode extends CstNode {
  name: 'unannClassOrInterfaceType';
  children: UnannClassOrInterfaceTypeCtx;
}

export type UnannClassOrInterfaceTypeCtx = {
  unannClassType: [UnannClassTypeCstNode];
};

export interface UnannClassTypeCstNode extends CstNode {
  name: 'unannClassType';
  children: UnannClassTypeCtx;
}

export type UnannClassTypeCtx = MultiIdentifier &
  Opt<{
    typeArguments: [TypeArgumentsCstNode];
  }> &
  OptAnnotations;

export interface UnannInterfaceTypeCstNode extends CstNode {
  name: 'unannInterfaceType';
  children: UnannInterfaceTypeCtx;
}

export type UnannInterfaceTypeCtx = {
  unannClassType: UnannClassTypeCstNode[];
};

export interface UnannTypeVariableCstNode extends CstNode {
  name: 'unannTypeVariable';
  children: UnannTypeVariableCtx;
}

export type UnannTypeVariableCtx = {
  Identifier: [IToken];
};

export interface MethodDeclarationCstNode extends CstNode {
  name: 'methodDeclaration';
  children: MethodDeclarationCtx;
}

export type MethodDeclarationCtx = {
  methodHeader: [MethodHeaderCstNode];
  methodBody: [MethodBodyCstNode];
} & Opt<{
  methodModifier: MethodModifierCstNode[];
}>;

export interface MethodModifierCstNode extends CstNode {
  name: 'methodModifier';
  children: MethodModifierCtx;
}

export type MethodModifierCtx =
  | {
      annotation: [AnnotationCstNode];
    }
  | {
      Public: [IToken];
    }
  | {
      Protected: [IToken];
    }
  | {
      Private: [IToken];
    }
  | {
      Abstract: [IToken];
    }
  | {
      Static: [IToken];
    }
  | {
      Synchronized: [IToken];
    }
  | {
      Native: [IToken];
    }
  | {
      Strictfp: [IToken];
    };

export interface MethodHeaderCstNode extends CstNode {
  name: 'methodHeader';
  children: MethodHeaderCtx;
}

export type MethodHeaderCtx = {
  result: [ResultCstNode];
  methodDeclarator: [MethodDeclaratorCstNode];
} & Opt<{ throws: [ThrowsCstNode] }> &
  Opt<{ typeParameters: TypeParametersCstNode[] } & OptAnnotations>;

export interface ResultCstNode extends CstNode {
  name: 'result';
  children: ResultCtx;
}

export type ResultCtx =
  | {
      unannType: [UnannTypeCstNode];
    }
  | {
      Void: [IToken];
    };

export interface MethodDeclaratorCstNode extends CstNode {
  name: 'methodDeclarator';
  children: MethodDeclaratorCtx;
}

export type MethodDeclaratorCtx = {
  Identifier: [IToken];
} & Brace &
  Opt<{
    dims: [DimsCstNode];
  }> &
  Opt<{
    formalParameterList: [FormalParameterListCstNode];
  }>;

export interface ReceiverParameterCstNode extends CstNode {
  name: 'receiverParameter';
  children: ReceiverParameterCtx;
}

export type ReceiverParameterCtx = {
  unannType: [UnannTypeCstNode];
  This: [IToken];
} & Opt<{
  Identifier: [IToken];
  Dot: [IToken];
}> &
  OptAnnotations;

export interface FormalParameterListCstNode extends CstNode {
  name: 'formalParameterList';
  children: FormalParameterListCtx;
}

export type FormalParameterListCtx = {
  formalParameter: [FormalParameterCstNode] & FormalParameterCstNode[];
} & OptCommas;

export interface FormalParameterCstNode extends CstNode {
  name: 'formalParameter';
  children: FormalParameterCtx;
}

export type FormalParameterCtx =
  | {
      variableParaRegularParameter: [VariableParaRegularParameterCstNode];
    }
  | {
      variableArityParameter: [VariableArityParameterCstNode];
    };

export interface VariableParaRegularParameterCstNode extends CstNode {
  name: 'variableParaRegularParameter';
  children: VariableParaRegularParameterCtx;
}

export type VariableParaRegularParameterCtx = {
  unannType: [UnannTypeCstNode];
  variableDeclaratorId: [VariableDeclaratorIdCstNode];
} & Opt<{
  variableModifier: VariableModifierCstNode[];
}>;

export interface VariableArityParameterCstNode extends CstNode {
  name: 'variableArityParameter';
  children: VariableArityParameterCtx;
}

export type VariableArityParameterCtx = {
  unannType: [UnannTypeCstNode];
  DotDotDot: [IToken];
  Identifier: [IToken];
} & {
  variableModifier: VariableModifierCstNode[];
} & OptAnnotations;

export interface VariableModifierCstNode extends CstNode {
  name: 'variableModifier';
  children: VariableModifierCtx;
}

export type VariableModifierCtx =
  | {
      annotation: [AnnotationCstNode];
    }
  | {
      Final: [IToken];
    };

export interface ThrowsCstNode extends CstNode {
  name: 'throws';
  children: ThrowsCtx;
}

export type ThrowsCtx = {
  Throws: [IToken];
  exceptionTypeList: [ExceptionTypeListCstNode];
};

export interface ExceptionTypeListCstNode extends CstNode {
  name: 'exceptionTypeList';
  children: ExceptionTypeListCtx;
}

export type ExceptionTypeListCtx = {
  exceptionType: [ExceptionTypeCstNode] & ExceptionTypeCstNode[];
} & OptCommas;

export interface ExceptionTypeCstNode extends CstNode {
  name: 'exceptionType';
  children: ExceptionTypeCtx;
}

export type ExceptionTypeCtx = {
  classType: [ClassTypeCstNode];
};

export interface MethodBodyCstNode extends CstNode {
  name: 'methodBody';
  children: MethodBodyCtx;
}

export type MethodBodyCtx =
  | {
      block: [BlockCstNode];
    }
  | {
      Semicolon: [IToken];
    };

export interface InstanceInitializerCstNode extends CstNode {
  name: 'instanceInitializer';
  children: InstanceInitializerCtx;
}

export type InstanceInitializerCtx = {
  block: [BlockCstNode];
};

export interface StaticInitializerCstNode extends CstNode {
  name: 'staticInitializer';
  children: StaticInitializerCtx;
}

export type StaticInitializerCtx = {
  Static: [IToken];
  block: [BlockCstNode];
};

export interface ConstructorDeclarationCstNode extends CstNode {
  name: 'constructorDeclaration';
  children: ConstructorDeclarationCtx;
}

export type ConstructorDeclarationCtx = {
  constructorDeclarator: [ConstructorDeclaratorCstNode];
  constructorBody: [ConstructorBodyCstNode];
} & Opt<{
  constructorModifier: ConstructorModifierCstNode[];
}> &
  Opt<{
    throws: [ThrowsCstNode];
  }>;

export interface ConstructorModifierCstNode extends CstNode {
  name: 'constructorModifier';
  children: ConstructorModifierCtx;
}

export type ConstructorModifierCtx =
  | {
      annotation: [AnnotationCstNode];
    }
  | {
      Public: [IToken];
    }
  | {
      Protected: [IToken];
    }
  | {
      Private: [IToken];
    };

export interface ConstructorDeclaratorCstNode extends CstNode {
  name: 'constructorDeclarator';
  children: ConstructorDeclaratorCtx;
}

export type ConstructorDeclaratorCtx = {
  simpleTypeName: [SimpleTypeNameCstNode];
} & Brace &
  Opt<{
    typeParameters: [TypeParametersCstNode];
  }> &
  Opt<{
    receiverParameter: [ReceiverParameterCstNode];
    Comma: [IToken];
  }> &
  Opt<{
    formalParameterList: [FormalParameterListCstNode];
  }>;

export interface SimpleTypeNameCstNode extends CstNode {
  name: 'simpleTypeName';
  children: SimpleTypeNameCtx;
}

export type SimpleTypeNameCtx = {
  Identifier: [IToken];
};

export interface ConstructorBodyCstNode extends CstNode {
  name: 'constructorBody';
  children: ConstructorBodyCtx;
}

export type ConstructorBodyCtx = Curly &
  Opt<{
    blockStatements: [BlockStatementsCstNode];
  }> &
  Opt<{
    explicitConstructorInvocation: [ExplicitConstructorInvocationCstNode];
  }>;

export interface ExplicitConstructorInvocationCstNode extends CstNode {
  name: 'explicitConstructorInvocation';
  children: ExplicitConstructorInvocationCtx;
}

export type ExplicitConstructorInvocationCtx =
  | {
      unqualifiedExplicitConstructorInvocation: [
        UnqualifiedExplicitConstructorInvocationCstNode
      ];
    }
  | {
      qualifiedExplicitConstructorInvocation: [
        QualifiedExplicitConstructorInvocationCstNode
      ];
    };

export interface UnqualifiedExplicitConstructorInvocationCstNode
  extends CstNode {
  name: 'unqualifiedExplicitConstructorInvocation';
  children: UnqualifiedExplicitConstructorInvocationCtx;
}

export type UnqualifiedExplicitConstructorInvocationCtx = {
  Semicolon: [IToken];
} & Brace &
  Opt<{
    typeArguments: [TypeArgumentsCstNode];
  }> &
  Opt<{
    argumentList: [ArgumentListCstNode];
  }> &
  (
    | {
        This: [IToken];
      }
    | {
        Super: [IToken];
      }
  );

export interface QualifiedExplicitConstructorInvocationCstNode extends CstNode {
  name: 'qualifiedExplicitConstructorInvocation';
  children: QualifiedExplicitConstructorInvocationCtx;
}

export type QualifiedExplicitConstructorInvocationCtx = {
  expressionName: [ExpressionNameCstNode];
  Dot: [IToken];
  Super: [IToken];
  Semicolon: [IToken];
} & Brace &
  Opt<{
    typeArguments: [TypeArgumentsCstNode];
  }> &
  Opt<{
    argumentList: [ArgumentListCstNode];
  }>;

export interface EnumDeclarationCstNode extends CstNode {
  name: 'enumDeclaration';
  children: EnumDeclarationCtx;
}

export type EnumDeclarationCtx = {
  Enum: [IToken];
  typeIdentifier: [TypeIdentifierCstNode];
  enumBody: [EnumBodyCstNode];
} & Opt<{
  classModifier: ClassModifierCstNode[];
}> &
  Opt<{
    superinterfaces: [SuperinterfacesCstNode];
  }>;

export interface EnumBodyCstNode extends CstNode {
  name: 'enumBody';
  children: EnumBodyCtx;
}

export type EnumBodyCtx = Curly &
  Opt<{
    enumConstantList: [EnumConstantListCstNode];
  }> &
  Opt<{
    enumBodyDeclarations: [EnumBodyDeclarationsCstNode];
  }> & {
    Comma: [IToken];
  };

export interface EnumConstantListCstNode extends CstNode {
  name: 'enumConstantList';
  children: EnumConstantListCtx;
}

export type EnumConstantListCtx = {
  enumConstant: [EnumConstantCstNode] & EnumConstantCstNode[];
} & OptCommas;

export interface EnumConstantCstNode extends CstNode {
  name: 'enumConstant';
  children: EnumConstantCtx;
}

export type EnumConstantCtx = {
  Identifier: [IToken];
} & Opt<{
  enumConstantModifier: EnumConstantModifierCstNode[];
}> &
  (Brace &
    Opt<{
      argumentList: [ArgumentListCstNode];
    }>) &
  Opt<{
    classBody: [ClassBodyCstNode];
  }>;

export interface EnumConstantModifierCstNode extends CstNode {
  name: 'enumConstantModifier';
  children: EnumConstantModifierCtx;
}

export type EnumConstantModifierCtx = {
  annotation: AnnotationCstNode[];
};

export interface EnumBodyDeclarationsCstNode extends CstNode {
  name: 'enumBodyDeclarations';
  children: EnumBodyDeclarationsCtx;
}

export type EnumBodyDeclarationsCtx = {
  Semicolon: [IToken];
} & Opt<{
  classBodyDeclaration: ClassBodyDeclarationCstNode[];
}>;

export interface IsClassDeclarationCstNode extends CstNode {
  name: 'isClassDeclaration';
  children: IsClassDeclarationCtx;
}

export type IsClassDeclarationCtx = Opt<{
  Semicolon: [IToken];
}> &
  Opt<{
    classModifier: ClassModifierCstNode[];
  }>;

export interface IdentifyClassBodyDeclarationTypeCstNode extends CstNode {
  name: 'identifyClassBodyDeclarationType';
  children: IdentifyClassBodyDeclarationTypeCtx;
}

export type IdentifyClassBodyDeclarationTypeCtx = {
  unannType: [UnannTypeCstNode];
} & Opt<{
  annotation: AnnotationCstNode[];
}> &
  Opt<{
    Public: IToken[];
  }> &
  Opt<{
    Protected: IToken[];
  }> &
  Opt<{
    Private: IToken[];
  }> &
  Opt<{
    Abstract: IToken[];
  }> &
  Opt<{
    Static: IToken[];
  }> &
  Opt<{
    Final: IToken[];
  }> &
  Opt<{
    Transient: IToken[];
  }> &
  Opt<{
    Volatile: IToken[];
  }> &
  Opt<{
    Synchronized: IToken[];
  }> &
  Opt<{
    Native: IToken[];
  }> &
  Opt<{
    Strictfp: IToken[];
  }>;

export interface IsDimsCstNode extends CstNode {
  name: 'isDims';
  children: IsDimsCtx;
}

export type IsDimsCtx = {
  At: IToken[];
} & {
  typeName: TypeNameCstNode[];
} & {
  LBrace: IToken[];
};

//Expressions

export interface ExpressionCstNode extends CstNode {
  name: 'expression';
  children: ExpressionCtx;
}

export type ExpressionCtx =
  | {
      lambdaExpression: [LambdaExpressionCstNode];
    }
  | {
      ternaryExpression: [TernaryExpressionCstNode];
    };

export interface LambdaExpressionCstNode extends CstNode {
  name: 'lambdaExpression';
  children: LambdaExpressionCtx;
}

export type LambdaExpressionCtx = {
  lambdaParameters: [LambdaParametersCstNode];
  Arrow: [IToken];
  lambdaBody: [LambdaBodyCstNode];
};

export interface LambdaParametersCstNode extends CstNode {
  name: 'lambdaParameters';
  children: LambdaParametersCtx;
}

export type LambdaParametersCtx =
  | {
      lambdaParametersWithBraces: [LambdaParametersWithBracesCstNode];
    }
  | {
      Identifier: [IToken];
    };

export interface LambdaParametersWithBracesCstNode extends CstNode {
  name: 'lambdaParametersWithBraces';
  children: LambdaParametersWithBracesCtx;
}

export type LambdaParametersWithBracesCtx = Brace &
  Opt<{
    lambdaParameterList: [LambdaParameterListCstNode];
  }>;

export interface LambdaParameterListCstNode extends CstNode {
  name: 'lambdaParameterList';
  children: LambdaParameterListCtx;
}

export type LambdaParameterListCtx =
  | {
      inferredLambdaParameterList: [InferredLambdaParameterListCstNode];
    }
  | {
      explicitLambdaParameterList: [ExplicitLambdaParameterListCstNode];
    };

export interface InferredLambdaParameterListCstNode extends CstNode {
  name: 'inferredLambdaParameterList';
  children: InferredLambdaParameterListCtx;
}

export type InferredLambdaParameterListCtx = {
  Identifier: IToken[];
} & OptCommas;

export interface ExplicitLambdaParameterListCstNode extends CstNode {
  name: 'explicitLambdaParameterList';
  children: ExplicitLambdaParameterListCtx;
}

export type ExplicitLambdaParameterListCtx = {
  lambdaParameter: LambdaParameterCstNode[];
} & OptCommas;

export interface LambdaParameterCstNode extends CstNode {
  name: 'lambdaParameter';
  children: LambdaParameterCtx;
}

export type LambdaParameterCtx =
  | {
      regularLambdaParameter: [RegularLambdaParameterCstNode];
    }
  | {
      variableArityParameter: [VariableArityParameterCstNode];
    };

export interface RegularLambdaParameterCstNode extends CstNode {
  name: 'regularLambdaParameter';
  children: RegularLambdaParameterCtx;
}

export type RegularLambdaParameterCtx = {
  lambdaParameterType: [LambdaParameterTypeCstNode];
  variableDeclaratorId: [VariableDeclaratorIdCstNode];
} & Opt<{
  variableModifier: VariableModifierCstNode[];
}>;

export interface LambdaParameterTypeCstNode extends CstNode {
  name: 'lambdaParameterType';
  children: LambdaParameterTypeCtx;
}

export type LambdaParameterTypeCtx =
  | {
      unannType: [UnannTypeCstNode];
    }
  | {
      Var: [IToken];
    };

export interface LambdaBodyCstNode extends CstNode {
  name: 'lambdaBody';
  children: LambdaBodyCtx;
}

export type LambdaBodyCtx =
  | {
      expression: [ExpressionCstNode];
    }
  | {
      block: [BlockCstNode];
    };

export interface TernaryExpressionCstNode extends CstNode {
  name: 'ternaryExpression';
  children: TernaryExpressionCtx;
}

export type TernaryExpressionCtx = {
  binaryExpression: [BinaryExpressionCstNode];
} & Opt<{
  QuestionMark: [IToken];
}>;

export interface BinaryExpressionCstNode extends CstNode {
  name: 'binaryExpression';
  children: BinaryExpressionCtx;
}

export type ShiftExpression = {
  unaryExpression: UnaryExpressionCstNode[];
} & (
  | {
      Less: IToken[];
    }
  | {
      Greater: IToken[];
    }
);

export type BinaryExpressionCtx = {
  unaryExpression: [UnaryExpressionCstNode];
} & (
  | {
      Instanceof: IToken[];
      referenceType: ReferenceTypeCstNode[];
    }
  | {
      AssignmentOperator: IToken[];
      expression: ExpressionCstNode[];
    }
  | {
      unaryExpression: UnaryExpressionCstNode[];
      BinaryOperator: IToken[];
    }
  | ShiftExpression
);

export interface UnaryExpressionCstNode extends CstNode {
  name: 'unaryExpression';
  children: UnaryExpressionCtx;
}

export type UnaryExpressionCtx = {
  primary: [PrimaryCstNode];
} & Opt<{
  UnaryPrefixOperator: IToken[];
}> &
  Opt<{
    UnarySuffixOperator: IToken[];
  }>;

export interface UnaryExpressionNotPlusMinusCstNode extends CstNode {
  name: 'unaryExpressionNotPlusMinus';
  children: UnaryExpressionNotPlusMinusCtx;
}

export type UnaryExpressionNotPlusMinusCtx = {
  primary: [PrimaryCstNode];
} & Opt<{
  UnaryPrefixOperatorNotPlusMinus: IToken[];
}> &
  Opt<{
    UnarySuffixOperator: IToken[];
  }>;

export interface PrimaryCstNode extends CstNode {
  name: 'primary';
  children: PrimaryCtx;
}

export type PrimaryCtx = {
  primaryPrefix: [PrimaryPrefixCstNode];
} & Opt<{
  primarySuffix: PrimarySuffixCstNode[];
}>;

export interface PrimaryPrefixCstNode extends CstNode {
  name: 'primaryPrefix';
  children: PrimaryPrefixCtx;
}

export type PrimaryPrefixCtx =
  | {
      literal: [LiteralCstNode];
    }
  | {
      This: [IToken];
    }
  | {
      Void: [IToken];
    }
  | {
      unannPrimitiveTypeWithOptionalDimsSuffix: [
        UnannPrimitiveTypeWithOptionalDimsSuffixCstNode
      ];
    }
  | {
      fqnOrRefType: [FqnOrRefTypeCstNode];
    }
  | {
      castExpression: [CastExpressionCstNode];
    }
  | {
      parenthesisExpression: [ParenthesisExpressionCstNode];
    }
  | {
      newExpression: [NewExpressionCstNode];
    }
  | {
      switchStatement: [SwitchStatementCstNode];
    };

export interface PrimarySuffixCstNode extends CstNode {
  name: 'primarySuffix';
  children: PrimarySuffixCtx;
}

export type TypeArgumentsSuffix = {
  Identifier: [IToken];
} & Opt<{
  typeArguments: [TypeArgumentsCstNode];
}>;

export type DotSuffix = {
  Dot: [IToken];
} & (
  | {
      This: [IToken];
    }
  | {
      unqualifiedClassInstanceCreationExpression: [
        UnqualifiedClassInstanceCreationExpressionCstNode
      ];
    }
  | TypeArgumentsSuffix
);

export type PrimarySuffixCtx =
  | DotSuffix
  | {
      methodInvocationSuffix: [MethodInvocationSuffixCstNode];
    }
  | {
      classLiteralSuffix: [ClassLiteralSuffixCstNode];
    }
  | {
      arrayAccessSuffix: [ArrayAccessSuffixCstNode];
    }
  | {
      methodReferenceSuffix: [MethodReferenceSuffixCstNode];
    };

export interface FqnOrRefTypeCstNode extends CstNode {
  name: 'fqnOrRefType';
  children: FqnOrRefTypeCtx;
}

export type FqnOrRefTypeCtx = {
  fqnOrRefTypePartFirst: [FqnOrRefTypePartFirstCstNode];
} & {
  Dot: IToken[];
  fqnOrRefTypePartRest: FqnOrRefTypePartRestCstNode[];
} & Opt<{
    dims: [DimsCstNode];
  }>;

export interface FqnOrRefTypePartRestCstNode extends CstNode {
  name: 'fqnOrRefTypePartRest';
  children: FqnOrRefTypePartRestCtx;
}

export type FqnOrRefTypePartRestCtx = {
  fqnOrRefTypePartCommon: [FqnOrRefTypePartCommonCstNode];
} & Opt<{
  typeArguments: [TypeArgumentsCstNode];
}> &
  Opt<{
    annotation: AnnotationCstNode[];
  }>;

export interface FqnOrRefTypePartCommonCstNode extends CstNode {
  name: 'fqnOrRefTypePartCommon';
  children: FqnOrRefTypePartCommonCtx;
}

export type FqnOrRefTypePartCommonCtx = (
  | {
      Identifier: [IToken];
    }
  | {
      Super: [IToken];
    }
) &
  Opt<{
    typeArguments: [TypeArgumentsCstNode];
  }>;

export interface FqnOrRefTypePartFirstCstNode extends CstNode {
  name: 'fqnOrRefTypePartFirst';
  children: FqnOrRefTypePartFirstCtx;
}

export type FqnOrRefTypePartFirstCtx = {
  fqnOrRefTypePartCommon: [FqnOrRefTypePartCommonCstNode];
} & Opt<{
  annotation: AnnotationCstNode[];
}>;

export interface ParenthesisExpressionCstNode extends CstNode {
  name: 'parenthesisExpression';
  children: ParenthesisExpressionCtx;
}

export type ParenthesisExpressionCtx = {
  expression: [ExpressionCstNode];
} & Brace;

export interface CastExpressionCstNode extends CstNode {
  name: 'castExpression';
  children: CastExpressionCtx;
}

export type CastExpressionCtx =
  | {
      primitiveCastExpression: [PrimitiveCastExpressionCstNode];
    }
  | {
      referenceTypeCastExpression: [ReferenceTypeCastExpressionCstNode];
    };

export interface PrimitiveCastExpressionCstNode extends CstNode {
  name: 'primitiveCastExpression';
  children: PrimitiveCastExpressionCtx;
}

export type PrimitiveCastExpressionCtx = {
  primitiveType: [PrimitiveTypeCstNode];
  unaryExpression: [UnaryExpressionCstNode];
} & Brace;

export interface ReferenceTypeCastExpressionCstNode extends CstNode {
  name: 'referenceTypeCastExpression';
  children: ReferenceTypeCastExpressionCtx;
}

export type ReferenceTypeCastExpressionCtx = {
  referenceType: [ReferenceTypeCstNode];
} & Brace &
  Opt<{
    additionalBound: AdditionalBoundCstNode[];
  }> &
  (
    | {
        lambdaExpression: [LambdaExpressionCstNode];
      }
    | {
        unaryExpressionNotPlusMinus: [UnaryExpressionNotPlusMinusCstNode];
      }
  );

export interface NewExpressionCstNode extends CstNode {
  name: 'newExpression';
  children: NewExpressionCtx;
}

export type NewExpressionCtx =
  | {
      arrayCreationExpression: [ArrayCreationExpressionCstNode];
    }
  | {
      unqualifiedClassInstanceCreationExpression: [
        UnqualifiedClassInstanceCreationExpressionCstNode
      ];
    };

export interface UnqualifiedClassInstanceCreationExpressionCstNode
  extends CstNode {
  name: 'unqualifiedClassInstanceCreationExpression';
  children: UnqualifiedClassInstanceCreationExpressionCtx;
}

export type UnqualifiedClassInstanceCreationExpressionCtx = {
  New: [IToken];
  classOrInterfaceTypeToInstantiate: [ClassOrInterfaceTypeToInstantiateCstNode];
} & Brace &
  Opt<{
    typeArguments: [TypeArgumentsCstNode];
  }> &
  Opt<{
    argumentList: [ArgumentListCstNode];
  }> &
  Opt<{
    classBody: [ClassBodyCstNode];
  }>;

export interface ClassOrInterfaceTypeToInstantiateCstNode extends CstNode {
  name: 'classOrInterfaceTypeToInstantiate';
  children: ClassOrInterfaceTypeToInstantiateCtx;
}

export type ClassOrInterfaceTypeToInstantiateCtx = MultiIdentifier &
  OptAnnotations &
  Opt<{
    typeArgumentsOrDiamond: [TypeArgumentsOrDiamondCstNode];
  }>;

export interface TypeArgumentsOrDiamondCstNode extends CstNode {
  name: 'typeArgumentsOrDiamond';
  children: TypeArgumentsOrDiamondCtx;
}

export type TypeArgumentsOrDiamondCtx =
  | {
      diamond: [DiamondCstNode];
    }
  | {
      typeArguments: [TypeArgumentsCstNode];
    };

export interface DiamondCstNode extends CstNode {
  name: 'diamond';
  children: DiamondCtx;
}

export type DiamondCtx = {
  Less: [IToken];
  Greater: [IToken];
};

export interface MethodInvocationSuffixCstNode extends CstNode {
  name: 'methodInvocationSuffix';
  children: MethodInvocationSuffixCtx;
}

export type MethodInvocationSuffixCtx = Brace &
  Opt<{
    argumentList: [ArgumentListCstNode];
  }>;

export interface ArgumentListCstNode extends CstNode {
  name: 'argumentList';
  children: ArgumentListCtx;
}

export type ArgumentListCtx = {
  expression: ExpressionCstNode[];
} & OptCommas;

export interface ArrayCreationExpressionCstNode extends CstNode {
  name: 'arrayCreationExpression';
  children: ArrayCreationExpressionCtx;
}

export type ArrayCreationExpressionCtx = {
  New: [IToken];
} & (
  | {
      primitiveType: [PrimitiveTypeCstNode];
    }
  | {
      classOrInterfaceType: [ClassOrInterfaceTypeCstNode];
    }
) &
  (
    | {
        arrayCreationDefaultInitSuffix: [ArrayCreationDefaultInitSuffixCstNode];
      }
    | {
        arrayCreationExplicitInitSuffix: [
          ArrayCreationExplicitInitSuffixCstNode
        ];
      }
  );

export interface ArrayCreationDefaultInitSuffixCstNode extends CstNode {
  name: 'arrayCreationDefaultInitSuffix';
  children: ArrayCreationDefaultInitSuffixCtx;
}

export type ArrayCreationDefaultInitSuffixCtx = {
  dimExprs: [DimExprsCstNode];
} & Opt<{
  dims: [DimsCstNode];
}>;

export interface ArrayCreationExplicitInitSuffixCstNode extends CstNode {
  name: 'arrayCreationExplicitInitSuffix';
  children: ArrayCreationExplicitInitSuffixCtx;
}

export type ArrayCreationExplicitInitSuffixCtx = {
  dims: [DimsCstNode];
  arrayInitializer: [ArrayInitializerCstNode];
};

export interface DimExprsCstNode extends CstNode {
  name: 'dimExprs';
  children: DimExprsCtx;
}

export type DimExprsCtx = {
  dimExpr: [DimExprCstNode];
};

export interface DimExprCstNode extends CstNode {
  name: 'dimExpr';
  children: DimExprCtx;
}

export type DimExprCtx = {
  LSquare: [IToken];
  expression: [ExpressionCstNode];
  RSquare: [IToken];
} & Opt<{
  annotation: AnnotationCstNode[];
}>;

export interface ClassLiteralSuffixCstNode extends CstNode {
  name: 'classLiteralSuffix';
  children: ClassLiteralSuffixCtx;
}

export type ClassLiteralSuffixCtx = {
  Dot: [IToken];
  Class: [IToken];
} & Opt<{
  LSquare: IToken[];
  RSquare: IToken[];
}>;

export interface ArrayAccessSuffixCstNode extends CstNode {
  name: 'arrayAccessSuffix';
  children: ArrayAccessSuffixCtx;
}

export type ArrayAccessSuffixCtx = {
  LSquare: [IToken];
  expression: [ExpressionCstNode];
  RSquare: [IToken];
};

export interface MethodReferenceSuffixCstNode extends CstNode {
  name: 'methodReferenceSuffix';
  children: MethodReferenceSuffixCtx;
}

export type MethodReferenceSuffixCtx = {
  ColonColon: [IToken];
} & ({ Identifier: [IToken] } | { New: [IToken] }) &
  Opt<{ typeArguments: [TypeArgumentsCstNode] }>;

export interface IdentifyNewExpressionTypeCstNode extends CstNode {
  name: 'identifyNewExpressionType';
  children: IdentifyNewExpressionTypeCtx;
}

export type IdentifyNewExpressionTypeCtx = {
  New: [IToken];
  classOrInterfaceTypeToInstantiate: [ClassOrInterfaceTypeToInstantiateCstNode];
};

export interface IsLambdaExpressionCstNode extends CstNode {
  name: 'isLambdaExpression';
  children: IsLambdaExpressionCtx;
}

// eslint-disable-next-line @typescript-eslint/ban-types
export type IsLambdaExpressionCtx = {};

export interface IsCastExpressionCstNode extends CstNode {
  name: 'isCastExpression';
  children: IsCastExpressionCtx;
}

// eslint-disable-next-line @typescript-eslint/ban-types
export type IsCastExpressionCtx = {};

export interface IsPrimitiveCastExpressionCstNode extends CstNode {
  name: 'isPrimitiveCastExpression';
  children: IsPrimitiveCastExpressionCtx;
}

export type IsPrimitiveCastExpressionCtx = {
  primitiveType: [PrimitiveTypeCstNode];
} & Brace;

export interface IsReferenceTypeCastExpressionCstNode extends CstNode {
  name: 'isReferenceTypeCastExpression';
  children: IsReferenceTypeCastExpressionCtx;
}

export type IsReferenceTypeCastExpressionCtx = {
  referenceType: [ReferenceTypeCstNode];
} & Brace &
  Opt<{
    additionalBound: AdditionalBoundCstNode[];
  }>;

export interface IsRefTypeInMethodRefCstNode extends CstNode {
  name: 'isRefTypeInMethodRef';
  children: IsRefTypeInMethodRefCtx;
}

export type IsRefTypeInMethodRefCtx = {
  typeArguments: [TypeArgumentsCstNode];
} & Opt<{ dims: [DimsCstNode] }> &
  Opt<{ Dot: [IToken] }>;

//Interfaces

export interface InterfaceDeclarationCstNode extends CstNode {
  name: 'interfaceDeclaration';
  children: InterfaceDeclarationCtx;
}

export type InterfaceDeclarationCtx = (
  | {
      normalInterfaceDeclaration: [NormalInterfaceDeclarationCstNode];
    }
  | {
      annotationTypeDeclaration: [AnnotationTypeDeclarationCstNode];
    }
) &
  Opt<{
    interfaceModifier: InterfaceModifierCstNode[];
  }>;

export interface NormalInterfaceDeclarationCstNode extends CstNode {
  name: 'normalInterfaceDeclaration';
  children: NormalInterfaceDeclarationCtx;
}

export type NormalInterfaceDeclarationCtx = {
  Interface: [IToken];
  typeIdentifier: [TypeIdentifierCstNode];
  interfaceBody: [InterfaceBodyCstNode];
} & Opt<{
  typeParameters: [TypeParametersCstNode];
}> &
  Opt<{
    extendsInterfaces: [ExtendsInterfacesCstNode];
  }>;

export interface InterfaceModifierCstNode extends CstNode {
  name: 'interfaceModifier';
  children: InterfaceModifierCtx;
}

export type InterfaceModifierCtx =
  | {
      annotation: [AnnotationCstNode];
    }
  | {
      Public: [IToken];
    }
  | {
      Protected: [IToken];
    }
  | {
      Private: [IToken];
    }
  | {
      Abstract: [IToken];
    }
  | {
      Static: [IToken];
    }
  | {
      Strictfp: [IToken];
    };

export interface ExtendsInterfacesCstNode extends CstNode {
  name: 'extendsInterfaces';
  children: ExtendsInterfacesCtx;
}

export type ExtendsInterfacesCtx = {
  Extends: [IToken];
  interfaceTypeList: [InterfaceTypeListCstNode];
};

export interface InterfaceBodyCstNode extends CstNode {
  name: 'interfaceBody';
  children: InterfaceBodyCtx;
}

export type InterfaceBodyCtx = Opt<{
  interfaceMemberDeclaration: InterfaceMemberDeclarationCstNode[];
}> &
  Curly;

export interface InterfaceMemberDeclarationCstNode extends CstNode {
  name: 'interfaceMemberDeclaration';
  children: InterfaceMemberDeclarationCtx;
}

export type InterfaceMemberDeclarationCtx =
  | {
      constantDeclaration: [ConstantDeclarationCstNode];
    }
  | {
      interfaceMethodDeclaration: [InterfaceMethodDeclarationCstNode];
    }
  | {
      classDeclaration: [ClassDeclarationCstNode];
    }
  | {
      interfaceDeclaration: [InterfaceDeclarationCstNode];
    }
  | {
      Semicolon: [IToken];
    };

export interface ConstantDeclarationCstNode extends CstNode {
  name: 'constantDeclaration';
  children: ConstantDeclarationCtx;
}

export type ConstantDeclarationCtx = {
  unannType: [UnannTypeCstNode];
  variableDeclaratorList: [VariableDeclaratorListCstNode];
  Semicolon: [IToken];
} & Opt<{
  constantModifier: ConstantModifierCstNode[];
}>;

export interface ConstantModifierCstNode extends CstNode {
  name: 'constantModifier';
  children: ConstantModifierCtx;
}

export type ConstantModifierCtx =
  | {
      annotation: [AnnotationCstNode];
    }
  | {
      Public: [IToken];
    }
  | {
      Static: [IToken];
    }
  | {
      Final: [IToken];
    };

export interface InterfaceMethodDeclarationCstNode extends CstNode {
  name: 'interfaceMethodDeclaration';
  children: InterfaceMethodDeclarationCtx;
}

export type InterfaceMethodDeclarationCtx = {
  methodHeader: [MethodHeaderCstNode];
  methodBody: [MethodBodyCstNode];
} & Opt<{
  interfaceMethodModifier: InterfaceMethodModifierCstNode[];
}>;

export interface InterfaceMethodModifierCstNode extends CstNode {
  name: 'interfaceMethodModifier';
  children: InterfaceMethodModifierCtx;
}

export type InterfaceMethodModifierCtx =
  | {
      annotation: [AnnotationCstNode];
    }
  | {
      Public: [IToken];
    }
  | {
      Private: [IToken];
    }
  | {
      Abstract: [IToken];
    }
  | {
      Default: [IToken];
    }
  | {
      Static: [IToken];
    }
  | {
      Strictfp: [IToken];
    };

export interface AnnotationTypeDeclarationCstNode extends CstNode {
  name: 'annotationTypeDeclaration';
  children: AnnotationTypeDeclarationCtx;
}

export type AnnotationTypeDeclarationCtx = {
  At: [IToken];
  Interface: [IToken];
  typeIdentifier: [TypeIdentifierCstNode];
  annotationTypeBody: [AnnotationTypeBodyCstNode];
};

export interface AnnotationTypeBodyCstNode extends CstNode {
  name: 'annotationTypeBody';
  children: AnnotationTypeBodyCtx;
}

export type AnnotationTypeBodyCtx = {
  annotationTypeMemberDeclaration: AnnotationTypeMemberDeclarationCstNode[];
} & Curly;

export interface AnnotationTypeMemberDeclarationCstNode extends CstNode {
  name: 'annotationTypeMemberDeclaration';
  children: AnnotationTypeMemberDeclarationCtx;
}

export type AnnotationTypeMemberDeclarationCtx =
  | {
      annotationTypeElementDeclaration: [
        AnnotationTypeElementDeclarationCstNode
      ];
    }
  | {
      constantDeclaration: [ConstantDeclarationCstNode];
    }
  | {
      classDeclaration: [ClassDeclarationCstNode];
    }
  | {
      interfaceDeclaration: [InterfaceDeclarationCstNode];
    }
  | {
      Semicolon: [IToken];
    };

export interface AnnotationTypeElementDeclarationCstNode extends CstNode {
  name: 'annotationTypeElementDeclaration';
  children: AnnotationTypeElementDeclarationCtx;
}

export type AnnotationTypeElementDeclarationCtx = {
  unannType: [UnannTypeCstNode];
  Identifier: [IToken];
  Semicolon: [IToken];
} & Opt<{
  dims: [DimsCstNode];
}> &
  Opt<{
    defaultValue: DefaultValueCstNode[];
  }> &
  Opt<{
    annotationTypeElementModifier: AnnotationTypeElementModifierCstNode[];
  }> &
  Brace;

export interface AnnotationTypeElementModifierCstNode extends CstNode {
  name: 'annotationTypeElementModifier';
  children: AnnotationTypeElementModifierCtx;
}

export type AnnotationTypeElementModifierCtx =
  | {
      annotation: [AnnotationCstNode];
    }
  | {
      Public: [IToken];
    }
  | {
      Abstract: [IToken];
    };

export interface DefaultValueCstNode extends CstNode {
  name: 'defaultValue';
  children: DefaultValueCtx;
}

export type DefaultValueCtx = {
  Default: [IToken];
  elementValue: [ElementValueCstNode];
};

export interface AnnotationCstNode extends CstNode {
  name: 'annotation';
  children: AnnotationCtx;
}

type AnnotationBody = Opt<
  | {
      elementValuePairList: [ElementValuePairListCstNode];
    }
  | {
      elementValue: [ElementValueCstNode];
    }
> &
  Brace;

export type AnnotationCtx = {
  At: [IToken];
  typeName: [TypeNameCstNode];
} & Opt<AnnotationBody>;

export interface ElementValuePairListCstNode extends CstNode {
  name: 'elementValuePairList';
  children: ElementValuePairListCtx;
}

export type ElementValuePairListCtx = {
  elementValuePair: ElementValuePairCstNode[];
} & OptCommas;

export interface ElementValuePairCstNode extends CstNode {
  name: 'elementValuePair';
  children: ElementValuePairCtx;
}

export type ElementValuePairCtx = {
  Identifier: [IToken];
  Equals: [IToken];
  elementValue: [ElementValueCstNode];
};

export interface ElementValueCstNode extends CstNode {
  name: 'elementValue';
  children: ElementValueCtx;
}

export type ElementValueCtx =
  | {
      expression: [ExpressionCstNode];
    }
  | {
      elementValueArrayInitializer: [ElementValueArrayInitializerCstNode];
    }
  | {
      annotation: [AnnotationCstNode];
    };

export interface ElementValueArrayInitializerCstNode extends CstNode {
  name: 'elementValueArrayInitializer';
  children: ElementValueArrayInitializerCtx;
}

export type ElementValueArrayInitializerCtx = Curly &
  Opt<{
    elementValueList: [ElementValueListCstNode];
  }> &
  Opt<{
    Comma: [IToken];
  }>;

export interface ElementValueListCstNode extends CstNode {
  name: 'elementValueList';
  children: ElementValueListCtx;
}

export type ElementValueListCtx = {
  elementValue: ElementValueCstNode[];
} & OptCommas;

export interface IdentifyInterfaceBodyDeclarationTypeCstNode extends CstNode {
  name: 'identifyInterfaceBodyDeclarationType';
  children: IdentifyInterfaceBodyDeclarationTypeCtx;
}

export type IdentifyInterfaceBodyDeclarationTypeCtx = {
  unannType: [UnannTypeCstNode];
} & Opt<{ Strictfp: IToken[] }> &
  Opt<{ annotation: AnnotationCstNode[] }> &
  Opt<{ Public: IToken[] }> &
  Opt<{ Protected: IToken[] }> &
  Opt<{ Private: IToken[] }> &
  Opt<{ Static: IToken[] }> &
  Opt<{ Final: IToken[] }> &
  Opt<{ Abstract: IToken[] }> &
  Opt<{ Default: IToken[] }>;

export interface IdentifyAnnotationBodyDeclarationTypeCstNode extends CstNode {
  name: 'identifyAnnotationBodyDeclarationType';
  children: IdentifyAnnotationBodyDeclarationTypeCtx;
}

export type IdentifyAnnotationBodyDeclarationTypeCtx = {
  unannType: [UnannTypeCstNode];
} & OptAnnotations &
  Opt<{
    Public: IToken[];
  }> &
  Opt<{
    Protected: IToken[];
  }> &
  Opt<{
    Private: IToken[];
  }> &
  Opt<{
    Static: IToken[];
  }> &
  Opt<{
    Final: IToken[];
  }> &
  Opt<{
    Abstract: IToken[];
  }> &
  Opt<{
    Strictfp: IToken[];
  }>;

export interface IsSimpleElementValueAnnotationCstNode extends CstNode {
  name: 'isSimpleElementValueAnnotation';
  children: IsSimpleElementValueAnnotationCtx;
}

export type IsSimpleElementValueAnnotationCtx = {
  annotation: [AnnotationCstNode];
};

// Lexical structures

export interface LiteralCstNode extends CstNode {
  name: 'literal';
  children: LiteralCtx;
}

export type LiteralCtx =
  | {
      integerLiteral: [IntegerLiteralCstNode];
    }
  | {
      floatingPointLiteral: [FloatingPointLiteralCstNode];
    }
  | {
      booleanLiteral: [BooleanLiteralCstNode];
    }
  | {
      CharLiteral: [IToken];
    }
  | {
      TextBlock: [IToken];
    }
  | {
      StringLiteral: [IToken];
    }
  | {
      Null: [IToken];
    };

export interface IntegerLiteralCstNode extends CstNode {
  name: 'integerLiteral';
  children: IntegerLiteralCtx;
}

export type IntegerLiteralCtx =
  | {
      DecimalLiteral: [IToken];
    }
  | {
      HexLiteral: [IToken];
    }
  | {
      OctalLiteral: [IToken];
    }
  | {
      BinaryLiteral: [IToken];
    };

export interface FloatingPointLiteralCstNode extends CstNode {
  name: 'floatingPointLiteral';
  children: FloatingPointLiteralCtx;
}

export type FloatingPointLiteralCtx =
  | {
      FloatLiteral: [IToken];
    }
  | {
      HexFloatLiteral: [IToken];
    };

export interface BooleanLiteralCstNode extends CstNode {
  name: 'booleanLiteral';
  children: BooleanLiteralCtx;
}

export type BooleanLiteralCtx =
  | {
      True: [IToken];
    }
  | {
      False: [IToken];
    };

// Names

export interface ModuleNameCstNode extends CstNode {
  name: 'moduleName';
  children: ModuleNameCtx;
}

export type ModuleNameCtx = MultiIdentifier;

export interface PackageNameCstNode extends CstNode {
  name: 'packageName';
  children: PackageNameCtx;
}

export type PackageNameCtx = MultiIdentifier;

export interface TypeNameCstNode extends CstNode {
  name: 'typeName';
  children: TypeNameCtx;
}

export type TypeNameCtx = MultiIdentifier;

export interface ExpressionNameCstNode extends CstNode {
  name: 'expressionName';
  children: ExpressionNameCtx;
}

export type ExpressionNameCtx = MultiIdentifier;

export interface MethodNameCstNode extends CstNode {
  name: 'methodName';
  children: MethodNameCtx;
}

export type MethodNameCtx = {
  Identifier: [IToken];
};

export interface PackageOrTypeNameCstNode extends CstNode {
  name: 'packageOrTypeName';
  children: PackageOrTypeNameCtx;
}

export type PackageOrTypeNameCtx = MultiIdentifier;

export interface AmbiguousNameCstNode extends CstNode {
  name: 'ambiguousName';
  children: AmbiguousNameCtx;
}

export type AmbiguousNameCtx = MultiIdentifier;

//Packages and modules

export interface CompilationUnitCstNode extends CstNode {
  name: 'compilationUnit';
  children: CompilationUnitCtx;
}

export type CompilationUnitCtx = {
  EOF: [IToken];
} & (
  | {
      modularCompilationUnit: [ModularCompilationUnitCstNode];
    }
  | {
      ordinaryCompilationUnit: [OrdinaryCompilationUnitCstNode];
    }
);

export interface OrdinaryCompilationUnitCstNode extends CstNode {
  name: 'ordinaryCompilationUnit';
  children: OrdinaryCompilationUnitCtx;
}

export type OrdinaryCompilationUnitCtx = Opt<{
  packageDeclaration: [PackageDeclarationCstNode];
}> &
  Opt<{
    importDeclaration: ImportDeclarationCstNode[];
  }> &
  Opt<{
    typeDeclaration: TypeDeclarationCstNode[];
  }>;

export interface ModularCompilationUnitCstNode extends CstNode {
  name: 'modularCompilationUnit';
  children: ModularCompilationUnitCtx;
}

export type ModularCompilationUnitCtx = {
  moduleDeclaration: [ModuleDeclarationCstNode];
} & Opt<{
  importDeclaration: ImportDeclarationCstNode[];
}>;

export interface PackageDeclarationCstNode extends CstNode {
  name: 'packageDeclaration';
  children: PackageDeclarationCtx;
}

export type PackageDeclarationCtx = MultiIdentifier & {
  Package: [IToken];
  Semicolon: [IToken];
} & Opt<{
    packageModifier: PackageModifierCstNode[];
  }>;

export interface PackageModifierCstNode extends CstNode {
  name: 'packageModifier';
  children: PackageModifierCtx;
}

export type PackageModifierCtx = {
  annotation: [AnnotationCstNode];
};

export interface ImportDeclarationCstNode extends CstNode {
  name: 'importDeclaration';
  children: ImportDeclarationCtx;
}

export type ImportDeclarationCtx =
  | ({
      Import: [IToken];
      packageOrTypeName: [PackageOrTypeNameCstNode];
      Semicolon: [IToken];
    } & Opt<{
      Static: [IToken];
    }> &
      Opt<{
        Dot: [IToken];
        Star: [IToken];
      }>)
  | {
      emptyStatement: [EmptyStatementCstNode];
    };

export interface TypeDeclarationCstNode extends CstNode {
  name: 'typeDeclaration';
  children: TypeDeclarationCtx;
}

export type TypeDeclarationCtx =
  | {
      classDeclaration: [ClassDeclarationCstNode];
    }
  | {
      interfaceDeclaration: [InterfaceDeclarationCstNode];
    }
  | {
      Semicolon: [IToken];
    };

export interface ModuleDeclarationCstNode extends CstNode {
  name: 'moduleDeclaration';
  children: ModuleDeclarationCtx;
}

export type ModuleDeclarationCtx = {
  Module: [IToken];
} & Opt<{
  Open: [IToken];
}> &
  Opt<{
    moduleDirective: ModuleDirectiveCstNode[];
  }> &
  OptAnnotations &
  MultiIdentifier &
  Curly;

export interface ModuleDirectiveCstNode extends CstNode {
  name: 'moduleDirective';
  children: ModuleDirectiveCtx;
}

export type ModuleDirectiveCtx =
  | {
      requiresModuleDirective: [RequiresModuleDirectiveCstNode];
    }
  | {
      exportsModuleDirective: [ExportsModuleDirectiveCstNode];
    }
  | {
      opensModuleDirective: [OpensModuleDirectiveCstNode];
    }
  | {
      usesModuleDirective: [UsesModuleDirectiveCstNode];
    }
  | {
      providesModuleDirective: [ProvidesModuleDirectiveCstNode];
    };

export interface RequiresModuleDirectiveCstNode extends CstNode {
  name: 'requiresModuleDirective';
  children: RequiresModuleDirectiveCtx;
}

export type RequiresModuleDirectiveCtx = {
  Requires: [IToken];
  moduleName: [ModuleNameCstNode];
  Semicolon: [IToken];
} & Opt<{
  requiresModifier: RequiresModifierCstNode[];
}>;

export interface ExportsModuleDirectiveCstNode extends CstNode {
  name: 'exportsModuleDirective';
  children: ExportsModuleDirectiveCtx;
}

export type OptMultiModuleName = Opt<
  {
    To: [IToken];
    moduleName: [ModuleNameCstNode];
  } & Opt<{
    Comma: IToken[];
    moduleName: ModuleNameCstNode[];
  }>
>;

export type ExportsModuleDirectiveCtx = {
  Exports: [IToken];
  packageName: [PackageNameCstNode];
  Semicolon: [IToken];
} & OptMultiModuleName;

export interface OpensModuleDirectiveCstNode extends CstNode {
  name: 'opensModuleDirective';
  children: OpensModuleDirectiveCtx;
}

export type OpensModuleDirectiveCtx = {
  Opens: [IToken];
  packageName: [PackageNameCstNode];
  Semicolon: [IToken];
} & OptMultiModuleName;

export interface UsesModuleDirectiveCstNode extends CstNode {
  name: 'usesModuleDirective';
  children: UsesModuleDirectiveCtx;
}

export type UsesModuleDirectiveCtx = {
  Uses: [IToken];
  typeName: [TypeNameCstNode];
  Semicolon: [IToken];
};

export interface ProvidesModuleDirectiveCstNode extends CstNode {
  name: 'providesModuleDirective';
  children: ProvidesModuleDirectiveCtx;
}

export type MultiTypeName = {
  typeName: [TypeNameCstNode];
} & Opt<{
  typeName: TypeNameCstNode[];
  Comma: IToken[];
}>;

export type ProvidesModuleDirectiveCtx = {
  Provides: [IToken];
  With: [IToken];
  Semicolon: [IToken];
} & MultiTypeName;

export interface RequiresModifierCstNode extends CstNode {
  name: 'requiresModifier';
  children: RequiresModifierCtx;
}

export type RequiresModifierCtx =
  | {
      Transitive: [IToken];
    }
  | {
      Static: [IToken];
    };

export interface IsModuleCompilationUnitCstNode extends CstNode {
  name: 'isModuleCompilationUnit';
  children: IsModuleCompilationUnitCtx;
}

export type IsModuleCompilationUnitCtx = Opt<{
  importDeclaration: ImportDeclarationCstNode[];
}> &
  Opt<{
    packageDeclaration: [PackageDeclarationCstNode];
  }> &
  OptAnnotations;

// Type Values And Variables

export interface TypeIdentifierCstNode extends CstNode {
  name: 'typeIdentifier';
  children: TypeIdentifierCtx;
}

export type TypeIdentifierCtx = {
  Identifier: [IToken];
};

export interface PrimitiveTypeCstNode extends CstNode {
  name: 'primitiveType';
  children: PrimitiveTypeCtx;
}

export type PrimitiveTypeCtx = (
  | {
      numericType: [NumericTypeCstNode];
    }
  | {
      Boolean: [IToken];
    }
) &
  OptAnnotations;

export interface NumericTypeCstNode extends CstNode {
  name: 'numericType';
  children: NumericTypeCtx;
}

export type NumericTypeCtx =
  | {
      integralType: [IntegralTypeCstNode];
    }
  | {
      floatingPointType: [FloatingPointTypeCstNode];
    };

export interface IntegralTypeCstNode extends CstNode {
  name: 'integralType';
  children: IntegralTypeCtx;
}

export type IntegralTypeCtx =
  | {
      Byte: [IToken];
    }
  | {
      Short: [IToken];
    }
  | {
      Int: [IToken];
    }
  | {
      Long: [IToken];
    }
  | {
      Char: [IToken];
    };

export interface FloatingPointTypeCstNode extends CstNode {
  name: 'floatingPointType';
  children: FloatingPointTypeCtx;
}

export type FloatingPointTypeCtx =
  | {
      Float: [IToken];
    }
  | {
      Double: [IToken];
    };

export interface ReferenceTypeCstNode extends CstNode {
  name: 'referenceType';
  children: ReferenceTypeCtx;
}

export type ReferenceTypeCtx = (
  | ({
      classOrInterfaceType: [ClassOrInterfaceTypeCstNode];
    } & Opt<{
      dims: [DimsCstNode];
    }>)
  | {
      primitiveType: [PrimitiveTypeCstNode];
      dims: [DimsCstNode];
    }
) &
  OptAnnotations;

export interface ClassOrInterfaceTypeCstNode extends CstNode {
  name: 'classOrInterfaceType';
  children: ClassOrInterfaceTypeCtx;
}

export type ClassOrInterfaceTypeCtx = {
  classType: [ClassTypeCstNode];
};

export interface ClassTypeCstNode extends CstNode {
  name: 'classType';
  children: ClassTypeCtx;
}

export type ClassTypeCtx = MultiIdentifier &
  Opt<{
    typeArguments: [TypeArgumentsCstNode];
  }> &
  OptAnnotations;

export interface InterfaceTypeCstNode extends CstNode {
  name: 'interfaceType';
  children: InterfaceTypeCtx;
}

export type InterfaceTypeCtx = {
  classType: ClassTypeCstNode[];
};

export interface TypeVariableCstNode extends CstNode {
  name: 'typeVariable';
  children: TypeVariableCtx;
}

export type TypeVariableCtx = {
  Identifier: [IToken];
} & OptAnnotations;

export interface DimsCstNode extends CstNode {
  name: 'dims';
  children: DimsCtx;
}

export type DimsCtx = {
  LSquare: IToken[];
  RSquare: IToken[];
} & OptAnnotations;

export interface TypeParameterCstNode extends CstNode {
  name: 'typeParameter';
  children: TypeParameterCtx;
}

export type TypeParameterCtx = {
  typeIdentifier: [TypeIdentifierCstNode];
} & Opt<{
  typeBound: [TypeBoundCstNode];
}> &
  Opt<{
    typeParameterModifier: TypeParameterModifierCstNode[];
  }>;

export interface TypeParameterModifierCstNode extends CstNode {
  name: 'typeParameterModifier';
  children: TypeParameterModifierCtx;
}

export type TypeParameterModifierCtx = {
  annotation: AnnotationCstNode[];
};

export interface TypeBoundCstNode extends CstNode {
  name: 'typeBound';
  children: TypeBoundCtx;
}

export type TypeBoundCtx = {
  Extends: [IToken];
  classOrInterfaceType: [ClassOrInterfaceTypeCstNode];
} & Opt<{
  additionalBound: AdditionalBoundCstNode[];
}>;

export interface AdditionalBoundCstNode extends CstNode {
  name: 'additionalBound';
  children: AdditionalBoundCtx;
}

export type AdditionalBoundCtx = {
  And: [IToken];
  interfaceType: [InterfaceTypeCstNode];
};

export interface TypeArgumentsCstNode extends CstNode {
  name: 'typeArguments';
  children: TypeArgumentsCtx;
}

export type TypeArgumentsCtx = {
  Less: [IToken];
  typeArgumentList: [TypeArgumentListCstNode];
  Greater: [IToken];
};

export interface TypeArgumentListCstNode extends CstNode {
  name: 'typeArgumentList';
  children: TypeArgumentListCtx;
}

export type TypeArgumentListCtx = {
  typeArgument: TypeArgumentCstNode[];
} & OptCommas;

export interface TypeArgumentCstNode extends CstNode {
  name: 'typeArgument';
  children: TypeArgumentCtx;
}

export type TypeArgumentCtx =
  | {
      referenceType: [ReferenceTypeCstNode];
    }
  | {
      wildcard: [WildcardCstNode];
    };

export interface WildcardCstNode extends CstNode {
  name: 'wildcard';
  children: WildcardCtx;
}

export type WildcardCtx = {
  QuestionMark: IToken[];
} & Opt<{
  wildcardBounds: [WildcardBoundsCstNode];
}> &
  OptAnnotations;

export interface WildcardBoundsCstNode extends CstNode {
  name: 'wildcardBounds';
  children: WildcardBoundsCtx;
}

export type WildcardBoundsCtx = {
  referenceType: [ReferenceTypeCstNode];
} & (
  | {
      Extends: [IToken];
    }
  | {
      Super: [IToken];
    }
);
