package org.devocative.metis.web.odata2;

import org.apache.olingo.odata2.api.edm.*;
import org.apache.olingo.odata2.api.uri.expression.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SQLExpressionVisitor implements ExpressionVisitor {
	private static final Logger logger = LoggerFactory.getLogger(SQLExpressionVisitor.class);

	private static final List<EdmSimpleType> INTEGER = Arrays.asList(
		EdmSimpleTypeKind.Int16.getEdmSimpleTypeInstance(),
		EdmSimpleTypeKind.Int32.getEdmSimpleTypeInstance(),
		EdmSimpleTypeKind.Int64.getEdmSimpleTypeInstance()
	);

	private static final List<EdmSimpleType> REAL = Arrays.asList(
		EdmSimpleTypeKind.Single.getEdmSimpleTypeInstance(),
		EdmSimpleTypeKind.Double.getEdmSimpleTypeInstance(),
		EdmSimpleTypeKind.Decimal.getEdmSimpleTypeInstance()
	);

	private static final List<EdmSimpleType> DATE = Arrays.asList(
		EdmSimpleTypeKind.DateTime.getEdmSimpleTypeInstance()
	);

	private static final List<EdmSimpleType> BOOLEAN = Arrays.asList(
		EdmSimpleTypeKind.Boolean.getEdmSimpleTypeInstance()
	);

	private static final List<EdmSimpleType> STRING = Arrays.asList(
		EdmSimpleTypeKind.String.getEdmSimpleTypeInstance()
	);

	private static final DateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm");
	private static final DateFormat DATE_FORMAT_SEC = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

	// ------------------------------

	private Map<String, Object> paramsValue = new HashMap<>();
	private int paramIndex = 0;

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
		String sqlOpr;

		switch (operator) {

			case OR:
				sqlOpr = "or";
				break;
			case AND:
				sqlOpr = "and";
				break;

			case EQ:
				sqlOpr = "=";
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

			default:
				logger.error("Unsupported operator: opr=[{}] expr=[{}]", operator.toUriLiteral(), binaryExpression.getUriLiteral());
				throw new RuntimeException("Unsupported operator: " + operator.toUriLiteral());
		}

		String left = createOperand(binaryExpression.getLeftOperand(), leftSide);
		String right = createOperand(binaryExpression.getRightOperand(), rightSide);

		return left + " " + sqlOpr + " " + right;
	}

	@Override
	public Object visitUnary(UnaryExpression unaryExpression, UnaryOperator operator, Object operand) {
		return null;
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
		throw new RuntimeException("SQLExpressionVisitor.Method no supported!");
	}

	@Override
	public Object visitMember(MemberExpression memberExpression, Object path, Object property) {
		throw new RuntimeException("SQLExpressionVisitor.Member no supported!");
	}

	// ------------------------------

	private String createOperand(CommonExpression expression, Object value) {
		String result;

		switch (expression.getKind()) {

			case LITERAL:
				paramsValue.put("p_" + paramIndex, value);
				result = ":p_" + paramIndex++;
				break;
			case PROPERTY:
				result = value.toString();
				break;

			default:
				logger.error("Not supported operand: type=[{}] expr=[{}]", expression.getKind(), expression.getUriLiteral());
				throw new RuntimeException("Not supported operand: " + expression.getUriLiteral());
		}

		return result;
	}

}
