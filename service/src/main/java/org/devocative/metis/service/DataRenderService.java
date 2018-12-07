package org.devocative.metis.service;

import org.devocative.adroit.date.EUniCalendar;
import org.devocative.adroit.date.TimeFieldVO;
import org.devocative.adroit.date.UniDate;
import org.devocative.adroit.vo.KeyValueVO;
import org.devocative.adroit.vo.RangeVO;
import org.devocative.metis.MetisConfigKey;
import org.devocative.metis.entity.data.DataSource;
import org.devocative.metis.entity.data.config.XDSFieldFilterType;
import org.devocative.metis.entity.data.config.XDSFieldType;
import org.devocative.metis.entity.data.config.XDataSource;
import org.devocative.metis.iservice.IDataRenderService;
import org.devocative.metis.iservice.IDataService;
import org.devocative.metis.iservice.data.IDataSourceService;
import org.devocative.metis.vo.DataAbstractFieldVO;
import org.devocative.metis.vo.DataVO;
import org.devocative.metis.vo.FilterInputParamsVO;
import org.devocative.metis.vo.FilterItemVO;
import org.devocative.metis.vo.FilterItemVO.ComponentType;
import org.devocative.metis.vo.FilterItemVO.PresentationMode;
import org.devocative.metis.vo.query.LookupQueryQVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service("mtsDataRenderService")
public class DataRenderService implements IDataRenderService {
	private static final Logger logger = LoggerFactory.getLogger(DataRenderService.class);

	private static final String DATE_PATTERN = "yyyyMMdd";
	private static final String DATE_TIME_PATTERN = "yyyyMMddHHmmss";

	// ------------------------------

	@Autowired
	private IDataService dataService;

	@Autowired
	private IDataSourceService dataSourceService;

	// ------------------------------

	@Override
	public List<FilterItemVO> createFilterItems(DataVO dataVO, FilterInputParamsVO inputParamsVO) {

		final List<String> disabledByInput = inputParamsVO.getAsStringList(DISABLED_FILTER_INPUT);
		final List<String> invisibleByInput = inputParamsVO.getAsStringList(INVISIBLE_FILTER_INPUT);
		final List<String> requiredByInput = inputParamsVO.getAsStringList(REQUIRED_FILTER_INPUT);

		final Map<String, List<String>> defaultMapByInput =
			inputParamsVO.getAsStringList(DEFAULT_FILTER_VALUE)
				.stream()
				.map(s -> s.split("[~]"))
				.filter(arr -> arr.length == 2 && !arr[0].isEmpty() && !arr[1].isEmpty())
				.collect(Collectors.groupingBy(
					o -> o[0].trim(),
					() -> new TreeMap<>(String.CASE_INSENSITIVE_ORDER),
					Collectors.mapping(o -> o[1].trim(), Collectors.toList())
				));

		final List<FilterItemVO> result = new ArrayList<>();

		for (DataAbstractFieldVO fieldVO : sortFilteringFields(dataVO.getAllFields())) {
			final String name = fieldVO.getName();
			final XDSFieldFilterType filterType = fieldVO.getFilterType();

			FilterItemVO item = new FilterItemVO()
				.setName(name)
				.setCaption(fieldVO.getTitleOrName());

			PresentationMode mode = PresentationMode.Default;
			if (invisibleByInput.contains(name) || !fieldVO.getInFilterPanelSafely()) {
				mode = PresentationMode.Invisible;
			} else if (disabledByInput.contains(name)) {
				mode = PresentationMode.Disabled;
			} else if (inputParamsVO.containsKey(name) ||
				(filterType == XDSFieldFilterType.Range &&
					(inputParamsVO.containsKey(name + "_u") || inputParamsVO.containsKey(name + "_l")))) {
				mode = PresentationMode.ExpectingSetData; // TIP: complex mode
			} else if (requiredByInput.contains(name) || fieldVO.getRequiredSafely()) {
				mode = PresentationMode.Required;
			}

			for (ComponentType value : ComponentType.values()) {
				if (value.match(fieldVO)) {
					item.setType(value);
					break;
				}
			}

			boolean invisibleItem = mode == PresentationMode.Invisible;
			Object defaultValue = processDefault(name, fieldVO.getType(), fieldVO.getFilterType(), inputParamsVO, defaultMapByInput, invisibleItem);
			final List<KeyValueVO<Serializable, String>> listValues = new ArrayList(); //TODO: just for test

			if (fieldVO.getType() == XDSFieldType.LookUp) {
				final boolean multiple = fieldVO.getTargetDSMultipleSelection() == null || fieldVO.getTargetDSMultipleSelection();
				item.setMultipleSelection(multiple);

				Map<String, Object> lookUpFilter = new HashMap<>();
				List defaultListValue = Collections.emptyList();

				try {
					final DataSource targetDS = dataSourceService.load(fieldVO.getTargetDSId());
					if (targetDS == null) {
						throw new RuntimeException(String.format("Target DataSource Not Found (maybe with name=[%s])", fieldVO.getTargetDSName()));
					}
					final XDataSource targetXDS = targetDS.getXDataSource();

					final Map<String, List<String>> map = fieldVO.getTargetDSFilter() != null ?
						Stream.of(fieldVO.getTargetDSFilter().split("[&]"))
							.map(s -> s.split("[=]"))
							.filter(s -> s.length == 2 && !s[0].isEmpty() && !s[1].isEmpty())
							.collect(Collectors.groupingBy(
								o -> o[0].trim(),
								() -> new TreeMap<>(String.CASE_INSENSITIVE_ORDER),
								Collectors.mapping(o -> o[1].trim(), Collectors.toList())
							)) :
						Collections.emptyMap();

					if (defaultValue != null && inputParamsVO.containsKey(fieldVO.getName())) {
						lookUpFilter.put(targetDS.getKeyField(), defaultValue);
					}

					switch (fieldVO.getFilterType()) {
						case List:
							targetXDS.getAllFields()
								.stream()
								.filter(it -> map.containsKey(it.getName()))
								.forEach(it -> lookUpFilter.put(it.getName(),
									processDefault(it.getName(), it.getType(), it.getFilterType(), new FilterInputParamsVO(), map, true)));

							LookupQueryQVO queryQVO = new LookupQueryQVO(dataVO.getDataSourceId(), fieldVO.getTargetDSId());
							queryQVO.setInputParams(lookUpFilter);
							listValues.addAll(dataSourceService.execute(queryQVO).getResult());

							if (inputParamsVO.containsKey(fieldVO.getName())) {
								defaultListValue = listValues;
							} else if (defaultMapByInput.containsKey(fieldVO.getName())) {
								List list = new ArrayList();
								for (String id : defaultMapByInput.get(fieldVO.getName())) {
									for (KeyValueVO<Serializable, String> kv : listValues) {
										if (kv.getKey().toString().equals(id)) {
											list.add(kv);
											break;
										}
									}
								}
								defaultListValue = list;
							}
							break;

						case Search:
							if (lookUpFilter.isEmpty() && defaultMapByInput.containsKey(fieldVO.getName())) {
								lookUpFilter.put(targetDS.getKeyField(), defaultMapByInput.get(fieldVO.getName()));
							}

							if (!lookUpFilter.isEmpty()) {
								LookupQueryQVO q = new LookupQueryQVO(dataVO.getDataSourceId(), fieldVO.getTargetDSId());
								q.setInputParams(lookUpFilter);
								defaultListValue = dataSourceService.execute(q).getResult();
							}

							item.setTargetSearchConfig(new FilterItemVO.TargetSearchConfig(targetXDS.getName(), map));
							break;

						default:
							throw new RuntimeException("Invalid");
					}

				} catch (Exception e) {
					logger.error("Preparing Lookup: SearchName=[{}] LookupField=[{}]", dataVO.getName(), name, e);

					final String err = String.format("%s(%s): %s", item.getCaption(), name, e.getMessage()); //TODO process error cause
					listValues.add(new KeyValueVO<>("?", err)); //TODO
					item.setError(err);
				}

				if (!defaultListValue.isEmpty()) {
					defaultValue = multiple ? defaultListValue : defaultListValue.get(0);
				}
			}

			item.setMode(mode)
				.setDefaultValue(defaultValue)
				.setListValues(listValues);

			result.add(item);
		}

		return result;
	}

	// ------------------------------

	private List<DataAbstractFieldVO> sortFilteringFields(List<DataAbstractFieldVO> allFields) {
		List<DataAbstractFieldVO> result = new ArrayList<>();

		int idx = 0;
		for (DataAbstractFieldVO fieldVO : allFields) {
			if (fieldVO.getInFilterPanelSafely()) {
				if (fieldVO.getFilterPanelOrder() == null) {
					fieldVO.setFilterPanelOrder(idx++);
				}
				result.add(fieldVO);
			}
		}

		for (DataAbstractFieldVO fieldVO : allFields) {
			if (!fieldVO.getInFilterPanelSafely() && fieldVO.getType() != XDSFieldType.Unknown) {
				fieldVO.setFilterPanelOrder(idx++);
				result.add(fieldVO);
			}
		}

		Collections.sort(result);

		return result;
	}

	private Object processDefault(
		String name,
		XDSFieldType type,
		XDSFieldFilterType filterType,
		FilterInputParamsVO inputParamsVO,
		Map<String, List<String>> defaultMapByInput,
		boolean invisible) {

		switch (filterType) {
			case Equal:
				if (inputParamsVO.containsKey(name)) {
					Object v = inputParamsVO.get(name);
					if (v instanceof Collection) {
						Collection col = (Collection) v;
						if (invisible && col.size() > 1) {
							return col.stream()
								.map(it -> convertQueryParam(type, it))
								.collect(Collectors.toList());
						} else {
							final Optional first = col.stream().findFirst();
							if (first.isPresent()) {
								return convertQueryParam(type, first.get());
							}
						}
					} else if (v instanceof KeyValueVO) {
						return convertQueryParam(type, ((KeyValueVO) v).getKey());
					}
					return convertQueryParam(type, v);
				} else if (defaultMapByInput.containsKey(name)) {
					/*
					Note: from fieldVO.getTargetDSFilter() it may be a list of filters
					 */
					final List<String> strings = defaultMapByInput.get(name);
					if (strings.size() == 1) {
						return convertQueryParam(type, strings.get(0));
					} else if (strings.size() > 1) {
						return strings
							.stream()
							.map(o -> convertQueryParam(type, o))
							.collect(Collectors.toList());
					}
				}
				break;

			case Contain:
			case TextSearch:
				if (inputParamsVO.containsKey(name)) {
					Object v = inputParamsVO.get(name);
					if (v instanceof Collection) {
						Collection col = (Collection) v;
						final Optional first = col.stream().findFirst();
						if (first.isPresent()) {
							return first.get().toString();
						}
					} else if (v instanceof KeyValueVO) {
						return ((KeyValueVO) v).getKey().toString();
					}
					return v.toString();
				} else if (defaultMapByInput.containsKey(name)) {
					return defaultMapByInput.get(name).get(0);
				}
				break;

			case Range:
				if (inputParamsVO.containsKey(name) && inputParamsVO.get(name) instanceof RangeVO) {
					return (RangeVO) inputParamsVO.get(name);
				}

				final String lowerKey = name + "_l";
				final List<?> lower =
					inputParamsVO.containsKey(lowerKey) ?
						inputParamsVO.getAsStringList(lowerKey) :
						defaultMapByInput.containsKey(lowerKey) ?
							defaultMapByInput.get(lowerKey) : Collections.emptyList();

				final String upperKey = name + "_u";
				final List<?> upper =
					inputParamsVO.containsKey(upperKey) ?
						inputParamsVO.getAsStringList(upperKey) :
						defaultMapByInput.containsKey(upperKey) ?
							defaultMapByInput.get(upperKey) : Collections.emptyList();

				if (!lower.isEmpty() || !upper.isEmpty()) {
					return new RangeVO<>(
						!lower.isEmpty() ? convertQueryParam(type, lower.get(0)) : null,
						!upper.isEmpty() ? convertQueryParam(type, upper.get(0)) : null
					);
				}
				break;

			case List:
			case Search:
				if (inputParamsVO.containsKey(name)) {
					Object value = inputParamsVO.get(name);
					if (value instanceof KeyValueVO) {
						return Collections.singletonList(((KeyValueVO) value).getKey());
					} else if (value instanceof Collection) {
						return ((Collection) value)
							.stream()
							.map(it -> {
								if (it instanceof KeyValueVO) {
									return ((KeyValueVO) it).getKey();
								}
								return it;
							}).collect(Collectors.toList());
					} else {
						return value;
					}
				} else if (defaultMapByInput.containsKey(name)) {
					return defaultMapByInput.get(name);
				}
				break;

			default:
				throw new RuntimeException("Invalid");
		}

		return null;
	}

	private Object convertQueryParam(XDSFieldType fieldType, Object value) {
		Object result = value;

		if (value != null && value instanceof String) {
			String str = (String) value;

			switch (fieldType) {
				case String:
					result = str;
					break;

				case Integer:
					result = Long.valueOf(str);
					break;

				case Real:
					result = new BigDecimal(str);
					break;

				case Date:
					final TimeFieldVO timeFieldVO = dataService.extractTimeFields(MetisConfigKey.FormDateDefaultTime);

					result = UniDate.of(EUniCalendar.Gregorian, str, DATE_PATTERN)
						.setTime(timeFieldVO)
						.toDate();
					break;

				case DateTime:
					result = UniDate.of(EUniCalendar.Gregorian, str, DATE_TIME_PATTERN).toDate();
					break;

				case Boolean:
					result = Boolean.valueOf(str);
					break;

				case LookUp:
					break;
			}
		}

		return result;
	}
}