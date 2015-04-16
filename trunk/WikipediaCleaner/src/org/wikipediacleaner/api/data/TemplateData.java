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
   * @param parameterName Parameter name.
   * @return Parameter for the given name.
   */
  public Parameter getParameter(String parameterName) {
    if ((parameterName == null) || (parameters == null)) {
      return null;
    }
    for (Parameter param : parameters) {
      if (param.isPossibleName(parameterName)) {
        return param;
      }
    }
    return null;
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
     * @return Description.
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
     * @return Description.
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

    /** Reason for deprecation. */
    private String deprecatedText;

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
      this.deprecatedText = null;
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
     * @param parameterName Parameter name.
     * @return True if the parameter name matches this template data parameter.
     */
    public boolean isPossibleName(String parameterName) {
      if (parameterName == null) {
        return false;
      }
      if ((name != null) && (name.equals(parameterName))) {
        return true;
      }
      if (aliases != null) {
        for (String alias : aliases) {
          if ((alias != null) && (alias.equals(parameterName))) {
            return true;
          }
        }
      }
      return false;
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
     * @return True if parameter is deprecated.
     */
    public boolean isDeprecated() {
      return deprecated;
    }

    /**
     * @return Reason for deprecation.
     */
    public String getDeprecatedText() {
      return deprecatedText;
    }

    /**
     * @param deprecated True if parameter is deprecated.
     */
    public void setDeprecated(boolean deprecated) {
      this.deprecated = deprecated;
      this.deprecatedText = null;
    }

    /**
     * @param text Reason for deprecation.
     */
    public void setDeprecated(String text) {
      if ((text != null) && (text.trim().length() > 0)) {
        this.deprecated = true;
        this.deprecatedText = text.trim();
      } else {
        this.deprecated = false;
        this.deprecatedText = null;
      }
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

    BOOLEAN("boolean"),
    CONTENT("content"),
    DATE("date"),
    LINE("line"),
    NUMBER("number"),
    STRING("string"),
    UNBALANCED_WIKITEXT("unbalanced-wikitext"),
    WIKI_FILE_NAME("wiki-file-name"),
    WIKI_PAGE_NAME("wiki-page-name"),
    WIKI_USER_NAME("wiki-user-name"),

    UNKNOWN("unknown");

    String type;

    /**
     * @param name Type name.
     */
    EnumParameterType(String name) {
      this.type = name;
    }

    /**
     * @param value Value.
     * @return True if the value is compatible with the type.
     * @see https://www.mediawiki.org/wiki/Extension:TemplateData#Format
     */
    public boolean isCompatible(String value) {
      if (value == null) {
        return true;
      }

      switch (this) {
      case CONTENT: // Page content in wikitext, such as text style, links, images, etc.
      case STRING: // Any textual value
      case UNBALANCED_WIKITEXT: // Raw wikitext that should not be treated as standalone content because it is unbalanced
      case WIKI_FILE_NAME: // A valid MediaWiki file name for the current wiki
      case WIKI_PAGE_NAME: // A valid MediaWiki page name for the current wiki
      case WIKI_USER_NAME: // A valid MediaWiki user name for the current wiki
        return true;

      case BOOLEAN: // A boolean value ('1' for true, '0' for false, '' for unknown)
        if (!value.trim().equals("0") &&
            !value.trim().equals("1") &&
            !value.trim().equals("")) {
          return false;
        }
        break;

      case DATE: // A date in ISO 8601 format, e.g. "2014-05-09" or "2014-05-09T16:01:12Z"
        // TODO
        // According to https://en.wikipedia.org/wiki/ISO_8601, some possible formats:
        //  Date:
        //   YYYY
        //   ±YYYYY
        //   YYYY-MM-DD or YYYYMMDD
        //   YYYY-MM
        //   YYYY-Www or YYYYWww
        //   YYYY-Www-D or YYYYWwwD
        //   YYYY-DDD or YYYYDDD
        //  Time: T for beginning time ; . or , are equivalent ; . or , can be used on the last item
        //   hh:mm:ss.sss or hhmmss.sss
        //   hh:mm:ss or hhmmss
        //   hh:mm or hhmm
        //   hh
        //  Time zone:
        //   <time>Z
        //   <time>±hh:mm
        //   <time>±hhmm
        //   <time>±hh
        break;

      case LINE: // Short text field - use for names, labels, and other short-form fields
        if (value.indexOf('\n') >= 0) {
          return false;
        }
        break;

      case NUMBER: // Any numerical value (without decimal points or thousand separators)
        // Check that it's a number
        for (int charNum = 0; charNum < value.length(); charNum++) {
          char currentChar = value.charAt(charNum);
          if (!Character.isDigit(currentChar) &&
              !Character.isWhitespace(currentChar) &&
              (currentChar != '.')) {
            return false;
          }
        }
        break;
      }

      return true;
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
