package org.devocative.metis.web.dPage.data;

import org.devocative.adroit.CalendarUtil;
import org.devocative.demeter.iservice.template.IToStringConverter;

import java.util.Date;

public class DateToStringConverter implements IToStringConverter<Date> {
	public static final IToStringConverter<Date> INSTANCE = new DateToStringConverter();

	@Override
	public String convertToString(Date obj) {
		return CalendarUtil.formatDate(obj, "yyyyMMddHHmmss");
	}
}
