package org.devocative.metis.web.odata2;

import org.apache.olingo.odata2.api.edm.*;
import org.apache.olingo.odata2.api.uri.expression.*;
import org.apache.olingo.odata2.core.edm.Uint7;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class SQLExpressionVisitor implements ExpressionVisitor {
	private static final Logger logger = LoggerFactory.getLogger(SQLExpressionVisitor.class);

	private static final List<EdmType> INTEGER = Arrays.asList(
		EdmSimpleTypeKind.Int16.getEdmSimpleTypeInstance(),
		EdmSimpleTypeKind.Int32.getEdmSimpleTypeInstance(),
		EdmSimpleTypeKind.Int64.getEdmSimpleTypeInstance(),
		Uint7.getInstance()
	);

	private static final List<EdmType> REAL = Arrays.asList(
		EdmSimpleTypeKind.Single.getEdmSimpleTypeInstance(),
		EdmSimpleTypeKind.Double.getEdmSimpleTypeInstance(),
		EdmSimpleTypeKind.Decimal.getEdmSimpleTypeInstance()
	);

	private static final List<EdmType> DATE = Collections.singletonList(
		EdmSimpleTypeKind.DateTime.getEdmSimpleTypeInstance()
	);

	private static final List<EdmType> BOOLEAN = Collections.singletonList(
		EdmSimpleTypeKind.Boolean.getEdmSimpleTypeInstance()
	);

	private static final List<EdmType> STRING = Collections.singletonList(
		EdmSimpleTypeKind.String.getEdmSimpleTypeInstance()
	);

	private static final DateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm");
	private static final DateFormat DATE_FORMAT_SEC = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

	// ------------------------------

	private Map<String, Object> paramsValue;
	private int paramIndex = 0;
	private List<String> mainQueryParams;

	// ------------------------------

	public SQLExpressionVisitor(Map<String, Object> paramsValue, List<String> mainQueryParams) {
		this.paramsValue = paramsValue;
		this.mainQueryParams = mainQueryParams;
	}

	// ------------------------------

	public Map<String, Object> getParamsValue() {
		return paramsValue;
	}

	// ------------------------------

	@Override
	public Object visitFilterExpression(FilterExpression filterExpression, String expressionString, Object expression) {
		return expression;
	}

	@Override
	public Object visitBinary(BinaryExpression binaryExpression, BinaryOperator operator, Object leftSide, Object rightSide) {
		String sqlOpr = "";
		boolean isOperator = true;

		switch (operator) {

			// ----- Logical Operators
			case OR:
				sqlOpr = "or";
				break;
			case AND:
				sqlOpr = "and";
				break;

			// ----- Relational Operator
			case EQ:
				if (isNullLiteral(binaryExpression.getRightOperand())) {
					sqlOpr = "is";
				} else if (!BOOLEAN.contains(binaryExpression.getLeftOperand().getEdmType()) &&
					!BOOLEAN.contains(binaryExpression.getRightOperand().getEdmType())) {
					sqlOpr = "=";
				}
				break;
			case NE:
				sqlOpr = "<>";
				break;
			case GE:
				sqlOpr = ">=";
				break;
			case GT:
				sqlOpr = ">";
				break;
			case LE:
				sqlOpr = "<=";
				break;
			case LT:
				sqlOpr = "<";
				break;

			// ----- Arithmetic Operators
			case ADD:
				sqlOpr = "+";
				break;
			case SUB:
				sqlOpr = "-";
				break;
			case MUL:
				sqlOpr = "*";
				break;
			case DIV:
				sqlOpr = "/";
				break;
			case MODULO:
				isOperator = false;
				sqlOpr = "mod";
				break;

			default:
				logger.error("Unsupported operator: opr=[{}] expr=[{}]", operator.toUriLiteral(), binaryExpression.getUriLiteral());
				throw new RuntimeException("Unsupported operator: " + operator.toUriLiteral());
		}

		String left = "";
		if (!BOOLEAN.contains(binaryExpression.getLeftOperand().getEdmType()) ||
			binaryExpression.getLeftOperand().getKind() != ExpressionKind.LITERAL) {
			left = createOperand(binaryExpression.getLeftOperand(), leftSide);
		}

		String right = "";
		if (!BOOLEAN.contains(binaryExpression.getRightOperand().getEdmType()) ||
			binaryExpression.getRightOperand().getKind() != ExpressionKind.LITERAL) {
			right = createOperand(binaryExpression.getRightOperand(), rightSide);
		}

		if (mainQueryParams.contains(left)) {
			if (right.startsWith(":")) {
				Object realValue = paramsValue.get(right.substring(1));
				paramsValue.put(left, realValue);
				return "1=1";
			} else {
				throw new RuntimeException("Using parameter as a column: " + left);
			}
		} else if (mainQueryParams.contains(right)) {
			if (left.startsWith(":")) {
				Object realValue = paramsValue.get(left.substring(1));
				paramsValue.put(right, realValue);
				return "1=1";
			} else {
				throw new RuntimeException("Using parameter as a column: " + right);
			}
		} else if (isOperator) {
			return String.format("(%s %s %s)", left, sqlOpr, right);
		} else {
			return String.format("%s(%s,%s)", sqlOpr, left, right);
		}
	}

	@Override
	public Object visitUnary(UnaryExpression unaryExpression, UnaryOperator operator, Object operand) {
		String sqlOpr = "";

		switch (operator) {
			case MINUS:
				sqlOpr = "-";
				break;
			case NOT:
				sqlOpr = "not";
				break;
		}
		return String.format("%s(%s)", sqlOpr, createOperand(unaryExpression.getOperand(), operand));
	}

	@Override
	public Object visitOrderByExpression(OrderByExpression orderByExpression, String expressionString, List<Object> orders) {
		throw new RuntimeException("SQLExpressionVisitor.OrderByExpression no supported!");
	}

	@Override
	public Object visitOrder(OrderExpression orderExpression, Object filterResult, SortOrder sortOrder) {
		throw new RuntimeException("SQLExpressionVisitor.Order no supported!");
	}

	@Override
	public Object visitLiteral(LiteralExpression literal, EdmLiteral edmLiteral) {
		Object result;
		String strValue = edmLiteral.getLiteral();

		try {
			if (INTEGER.contains(edmLiteral.getType())) {
				result = new Long(strValue);
			} else if (REAL.contains(edmLiteral.getType())) {
				result = new BigDecimal(strValue);
			} else if (DATE.contains(edmLiteral.getType())) {
				strValue = strValue.replaceAll("T", " ");
				if (strValue.length() == 16) {
					result = DATE_FORMAT.parse(strValue);
				} else {
					if (strValue.length() > 19) {
						strValue = strValue.substring(0, 19);
					}
					result = DATE_FORMAT_SEC.parse(strValue);
				}
			} else if (BOOLEAN.contains(edmLiteral.getType())) {
				result = Boolean.valueOf(strValue);
			} else if (STRING.contains(edmLiteral.getType())) {
				result = strValue;
			} else if (EdmSimpleTypeKind.Null.getEdmSimpleTypeInstance().equals(edmLiteral.getType())) {
				result = "null";
			} else {
				logger.error("Unknown literal type: type=[{}] lit=[{}]", edmLiteral.getType(), literal.getUriLiteral());
				result = strValue;
			}
		} catch (Exception e) {
			logger.error("Literal conversion error: type=[{}] lit=[{}]", edmLiteral.getType(), literal.getUriLiteral());
			result = strValue;
		}
		return result;
	}

	@Override
	public Object visitProperty(PropertyExpression propertyExpression, String uriLiteral, EdmTyped edmProperty) {
		try {
			return edmProperty.getName();
		} catch (EdmException e) {
			logger.error("SQLExpressionVisitor.Property", e);
			throw new RuntimeException(e);
		}
	}

	@Override
	public Object visitMethod(MethodExpression methodExpression, MethodOperator method, List<Object> parameters) {
		String firstParam, secondParam, thirdParam;
		int idx1 = 0, idx2 = 1;

		switch (method) {
			case LENGTH:
				return String.format("length(%s)", createOperand(methodExpression.getParameters().get(0), parameters.get(0)));

			case TOLOWER:
				return String.format("lower(%s)", createOperand(methodExpression.getParameters().get(0), parameters.get(0)));

			case TOUPPER:
				return String.format("upper(%s)", createOperand(methodExpression.getParameters().get(0), parameters.get(0)));

			case TRIM:
				return String.format("trim(%s)", createOperand(methodExpression.getParameters().get(0), parameters.get(0)));

			case INDEXOF:
				firstParam = createOperand(methodExpression.getParameters().get(idx1), parameters.get(idx1));
				secondParam = createOperand(methodExpression.getParameters().get(idx2), parameters.get(idx2));
				return String.format("instr(%s, %s)", firstParam, secondParam);

			case SUBSTRINGOF:
				idx1 = 1;
				idx2 = 0;
				firstParam = createOperand(methodExpression.getParameters().get(idx1), parameters.get(idx1));
				if (methodExpression.getParameters().get(idx2).getKind() == ExpressionKind.LITERAL) {
					secondParam = createOperand(methodExpression.getParameters().get(idx2), String.format("%%%s%%", parameters.get(idx2)));
					return String.format("%s like %s", firstParam, secondParam);
				} else {
					secondParam = createOperand(methodExpression.getParameters().get(idx2), parameters.get(idx2));
					return String.format("instr(%s, %s) > 0", firstParam, secondParam);
				}

			case STARTSWITH:
				firstParam = createOperand(methodExpression.getParameters().get(idx1), parameters.get(idx1));
				if (methodExpression.getParameters().get(idx2).getKind() == ExpressionKind.LITERAL) {
					secondParam = createOperand(methodExpression.getParameters().get(idx2), String.format("%s%%", parameters.get(idx2)));
					return String.format("%s like %s", firstParam, secondParam);
				} else {
					secondParam = createOperand(methodExpression.getParameters().get(idx2), parameters.get(idx2));
					return String.format("instr(%s, %s) = 1", firstParam, secondParam);
				}

			case ENDSWITH:
				firstParam = createOperand(methodExpression.getParameters().get(idx1), parameters.get(idx1));
				if (methodExpression.getParameters().get(idx2).getKind() == ExpressionKind.LITERAL) {
					secondParam = createOperand(methodExpression.getParameters().get(1), String.format("%%%s", parameters.get(idx2)));
					return String.format("%s like %s", firstParam, secondParam);
				} else {
					secondParam = createOperand(methodExpression.getParameters().get(idx2), parameters.get(idx2));
					return String.format("instr(%1$s, %2$s) = (length(%1$s) - length(%2$s) + 1)", firstParam, secondParam);
				}

			case CONCAT:
				firstParam = createOperand(methodExpression.getParameters().get(idx1), parameters.get(idx1));
				secondParam = createOperand(methodExpression.getParameters().get(idx2), parameters.get(idx2));
				return String.format("%s || %s", firstParam, secondParam);

			case SUBSTRING:
				firstParam = createOperand(methodExpression.getParameters().get(idx1), parameters.get(idx1));
				secondParam = createOperand(methodExpression.getParameters().get(idx2), parameters.get(idx2));
				if (parameters.size() == 2) {
					return String.format("substr(%s, %s)", firstParam, secondParam);
				} else {
					thirdParam = createOperand(methodExpression.getParameters().get(2), parameters.get(2));
					return String.format("substr(%s, %s, %s)", firstParam, secondParam, thirdParam);
				}
		}
		throw new RuntimeException("SQLExpressionVisitor.Method not supported: " + method);
	}

	@Override
	public Object visitMember(MemberExpression memberExpression, Object path, Object property) {
		throw new RuntimeException("SQLExpressionVisitor.Member no supported!");
	}

	// ------------------------------

	private String createOperand(CommonExpression expression, Object value) {
		String result;

		switch (expression.getKind()) {

			/*case FILTER:
				break;
			case MEMBER:
				break;
			case ORDER:
				break;
			case ORDERBY:
				break;*/
			case LITERAL:
				if (isNullLiteral(expression)) {
					result = "null";
				} else {
					paramsValue.put("p_" + paramIndex, value);
					result = ":p_" + paramIndex++;
				}
				break;
			case BINARY:
			case UNARY:
			case PROPERTY:
			case METHOD:
				result = value.toString();
				break;
			default:
				logger.error("Not supported operand: type=[{}] expr=[{}]", expression.getKind(), expression.getUriLiteral());
				throw new RuntimeException(String.format("Not supported operand: type=[%s] expr=[%s]",
					expression.getKind(), expression.getUriLiteral()));
		}

		return result;
	}

	private boolean isNullLiteral(CommonExpression expression) {
		if (expression instanceof LiteralExpression) {
			LiteralExpression literalExpression = (LiteralExpression) expression;
			return EdmSimpleTypeKind.Null.getEdmSimpleTypeInstance().equals(literalExpression.getEdmType());
		}
		return false;
	}

}
