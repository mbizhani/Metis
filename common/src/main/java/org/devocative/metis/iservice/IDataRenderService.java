package org.devocative.metis.iservice;

import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.FilterInputParamsVO;
import org.devocative.metis.vo.FilterItemVO;

import java.util.List;

public interface IDataRenderService {
	String DISABLED_FILTER_INPUT = "$disableFilter";
	String INVISIBLE_FILTER_INPUT = "$invisibleFilter";
	String REQUIRED_FILTER_INPUT = "$required";
	String DEFAULT_FILTER_VALUE = "$default";

	String WINDOW = "$window";
	String MULTI_SELECT = "$multiselect";
	String RETURN_FIELD = "$return";
	String DISABLE_SORT_COLUMN = "$disableSort";
	String DISABLE_SORT_ALL_COLUMN = "$disableAllSorts";
	String SEARCH_ON_START = "$searchOnStart";
	String ACTIONS = "$actions";
	String RETURN_VERSION = "$retVer"; // values: 1, 2

	List<FilterItemVO> createFilterItems(DataVO dataVO, FilterInputParamsVO inputParamsVO);
}
