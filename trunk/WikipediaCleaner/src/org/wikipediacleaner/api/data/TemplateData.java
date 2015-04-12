/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


/**
 * Bean for holding TemplateData information.
 * 
 * @see <a href="http://www.mediawiki.org/wiki/Extension:TemplateData">TemplateData extension</a>
 */
public class TemplateData {

  /**
   * Title of the template.
   */
  private String title;

  /**
   * Template description.
   */
  private InterfaceText description;

  /**
   * Template parameters.
   */
  private List<Parameter> parameters;

  /**
   * Default constructor.
   */
  public TemplateData() {
    this.description = null;
  }

  /**
   * Create a template data from a page analysis.
   * 
   * @param analysis Page analysis.
   * @return TemplateData.
   */
  public static TemplateData createFromContent(PageAnalysis analysis) {
    TemplateData result = new TemplateData();
    List<PageElementParameter> parameters = analysis.getParameters();
    if (parameters != null) {
      List<Parameter> params = new ArrayList<TemplateData.Parameter>();
      for (PageElementParameter parameter : parameters) {
        String name = parameter.getParameterName();
        Parameter param = null;
        for (Parameter tmpParam : params) {
          if (name.equals(tmpParam.getName())) {
            param = tmpParam;
          }
        }
        if (param == null) {
          param = new Parameter(name);
          params.add(param);
        }
      }
      Collections.sort(params);
      result.parameters = params;
    }
    return result;
  }

  /**
   * @return Name of the template.
   */
  public String getTitle() {
    return title;
  }

  /**
   * @param title Name of the template.
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * @return Template description.
   */
  public InterfaceText getDescription() {
    return description;
  }

  /**
   * @param description Template description.
   */
  public void setDescription(InterfaceText description) {
    this.description = description;
  }

  /**
   * @return Template parameters.
   */
  public List<Parameter> getParameters() {
    return parameters;
  }

  /**
   * @param parameters Template parameters.
   */
  public void setParameters(List<Parameter> parameters) {
    this.parameters = parameters;
  }

  /**
   * Bean for holding a description in a single language or several languages.
   */
  public static class InterfaceText {

    /**
     * True if multiple languages are used for the description.
     */
    private boolean multipleLanguages;

    /**
     * Description in a single language.
     */
    private String text;

    /**
     * Description in multiple languages.
     */
    private List<LanguageValue> texts;

    /**
     * Default constructor.
     */
    public InterfaceText() {
      this.multipleLanguages = false;
      this.text = null;
      this.texts = null;
    }

    /**
     * Constructor for a single language.
     * 
     * @param text Description. 
     */
    public InterfaceText(String text) {
      this.multipleLanguages = false;
      this.text = text;
      this.texts = null;
    }

    /**
     * Constructor for multiple languages.
     * 
     * @param texts Descriptions.
     */
    public InterfaceText(List<LanguageValue> texts) {
      this.multipleLanguages = true;
      this.text = null;
      this.texts = new ArrayList<LanguageValue>();
      if (texts != null) {
        this.texts.addAll(texts);
      }
    }

    /**
     * @return True if descriptions are in multiple languages.
     */
    public boolean hasMultipleLanguages() {
      return multipleLanguages;
    }

    /**
     * @return Description in a single language.
     */
    public String getText() {
      return text;
    }

    /**
     * @return Descriptions in multiple languages.
     */
    public List<LanguageValue> getTexts() {
      return Collections.unmodifiableList(texts);
    }

    /**
     * @return
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      if (!multipleLanguages) {
        return text;
      }
      if ((texts != null) && !texts.isEmpty()) {
        return texts.get(0).toString();
      }
      return super.toString();
    }
  }

  /**
   * Bean for holding a description in one specified language.
   */
  public static class LanguageValue {

    /**
     * Language code.
     */
    private String language;

    /**
     * Description.
     */
    private String text;

    /**
     * @param language Language code.
     * @param text Description in that language.
     */
    public LanguageValue(String language, String text) {
      this.language = language;
      this.text = text;
    }

    /**
     * @return Language code.
     */
    public String getLanguage() {
      return language;
    }

    /**
     * @return Description.
     */
    public String getText() {
      return text;
    }

    /**
     * @return
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      return "" + language + " - " + text;
    }
  }

  /**
   * Bean for holding a template parameter description.
   */
  public static class Parameter implements Comparable<Parameter> {

    /** Parameter name. */
    private String name;

    /** Label for the parameter. */
    private InterfaceText label;

    /** Parameter description. */
    private InterfaceText description;

    /** Parameter type. */
    private String type;

    /** Parameter aliases. */
    private List<String> aliases;

    /** True if parameter is required. */
    private boolean required;

    /** True if parameter should be suggested. */
    private boolean suggested;

    /** True if parameter is deprecated. */
    private boolean deprecated;

    /** Value to use when adding the parameter. */
    private String autoValue;

    /** Default value when parameter is not set. */
    private String defaultValue;

    /**
     * Default constructor.
     */
    public Parameter(String name) {
      this.name = name;
      this.label = new InterfaceText();
      this.description = new InterfaceText();
      this.type = null;
      this.aliases = null;
      this.required = false;
      this.suggested = false;
      this.deprecated = false;
      this.autoValue = null;
      this.defaultValue = null;
    }

    /**
     * @return Parameter name.
     */
    public String getName() {
      return name;
    }

    /**
     * @return Parameter label.
     */
    public InterfaceText getLabel() {
      return label;
    }

    /**
     * @param label Parameter label.
     */
    public void setLabel(InterfaceText label) {
      this.label = label;
    }

    /**
     * @return Parameter description.
     */
    public InterfaceText getDescription() {
      return description;
    }

    /**
     * @param description Parameter description.
     */
    public void setDescription(InterfaceText description) {
      this.description = description;
    }

    /**
     * @return Parameter type.
     */
    public EnumParameterType getType() {
      for (EnumParameterType paramType : EnumParameterType.values()) {
        if (paramType.type.equals(type)) {
          return paramType;
        }
      }
      return EnumParameterType.UNKNOWN;
    }

    /**
     * @param type Parameter type.
     */
    public void setType(String type) {
      this.type = type;
    }

    /**
     * @return Parameter aliases.
     */
    public List<String> getAliases() {
      return aliases;
    }

    /**
     * @param aliases Parameter aliases.
     */
    public void setAliases(List<String> aliases) {
      this.aliases = aliases;
    }

    /**
     * @return True if parameter is required.
     */
    public boolean isRequired() {
      return required;
    }

    /**
     * @param required True if parameter is required.
     */
    public void setRequired(boolean required) {
      this.required = required;
    }

    /**
     * @return True if parameter should be suggested.
     */
    public boolean isSuggested() {
      return suggested;
    }

    /**
     * @param suggested True if parameter should be suggested.
     */
    public void setSuggested(boolean suggested) {
      this.suggested = suggested;
    }

    /**
     * @return if parameter is deprecated.
     */
    public boolean isDeprecated() {
      return deprecated;
    }

    /**
     * @param deprecated True if parameter is deprecated.
     */
    public void setDeprecated(boolean deprecated) {
      this.deprecated = deprecated;
    }

    /**
     * @return Parameter auto value.
     */
    public String getAutoValue() {
      return autoValue;
    }

    /**
     * @param autoValue Parameter auto value.
     */
    public void setAutoValue(String autoValue) {
      this.autoValue = autoValue;
    }

    /**
     * @return Parameter default value.
     */
    public String getDefaultValue() {
      return defaultValue;
    }

    /**
     * @param defaultValue Parameter default value.
     */
    public void setDefaultValue(String defaultValue) {
      this.defaultValue = defaultValue;
    }

    /**
     * @param o The TemplateData to be compared.
     * @return  a negative integer, zero, or a positive integer as this TemplateData
     *    is less than, equal to, or greater than the specified TemplateData.
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    public int compareTo(Parameter o) {
      return name.compareTo(o.name);
    }

    /**
     * @return Description.
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      if (name != null) {
        return name;
      }
      return super.toString();
    }
  }

  /**
   * Enumeration of all parameter types.
   */
  public static enum EnumParameterType {

    NUMBER("number"),
    STRING("string"),
    UNKNOWN("unknown"),
    WIKI_PAGE_NAME("string/wiki-page-name"),
    WIKI_USER_NAME("string/wiki-user-name");

    String type;

    EnumParameterType(String name) {
      this.type = name;
    }

    /**
     * @return Description.
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString() {
      return type;
    }
  }

  /**
   * Bean for holding a template set.
   */
  public static class Set {

    /**
     * Set label.
     */
    private InterfaceText label;

    /**
     * List of parameters in the set.
     */
    private List<String> parameters;

    /**
     * Default constructor.
     */
    public Set() {
      this.label = new InterfaceText();
      this.parameters = null;
    }

    /**
     * @return Set label.
     */
    public InterfaceText getLabel() {
      return label;
    }

    /**
     * @return List of parameters in the set.
     */
    public List<String> getParameters() {
      return parameters;
    }
  }
}
