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
          param = new Parameter();
          params.add(param);
        }
      }
      Collections.sort(params);
      result.parameters = params;
    }
    return result;
  }

  /**
   * @return Template description.
   */
  public InterfaceText getDescription() {
    return description;
  }

  /**
   * @return Template parameters.
   */
  public List<Parameter> getParameters() {
    return parameters;
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
  }

  /**
   * Bean for holding a template parameter description.
   */
  public static class Parameter implements Comparable<Parameter> {

    /**
     * Parameter name.
     */
    private String name;

    private InterfaceText label;

    private boolean required;

    private InterfaceText description;

    private String deprecated;

    private List<String> aliases;

    private String defaultValue;

    private String type;

    private String inherits;

    /**
     * Default constructor.
     */
    public Parameter() {
      this.name = null;
      this.label = new InterfaceText();
      this.required = false;
      this.description = new InterfaceText();
      this.deprecated = null;
      this.aliases = null;
      this.defaultValue = null;
      this.type = null;
      this.inherits = null;
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
     * @return True if parameter is required.
     */
    public boolean isRequired() {
      return required;
    }

    /**
     * @return Parameter description.
     */
    public InterfaceText getDescription() {
      return description;
    }

    /**
     * @return True or text if parameter is deprecated.
     */
    public String getDeprecated() {
      return deprecated;
    }

    /**
     * @return Parameter aliases.
     */
    public List<String> getAliases() {
      return aliases;
    }

    /**
     * @return Parameter default value.
     */
    public String getDefaultValue() {
      return defaultValue;
    }

    /**
     * @return Parameter type.
     */
    public String getType() {
      return type;
    }

    /**
     * @return Parameter inheritance.
     */
    public String getInherits() {
      return inherits;
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
