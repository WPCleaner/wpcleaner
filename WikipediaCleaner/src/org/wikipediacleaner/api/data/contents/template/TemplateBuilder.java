/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.template;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.data.PageElementTemplate;

/**
 * Builder class.
 */
public class TemplateBuilder {

  /** Template name */
  @Nonnull
  private final String name;

  /** Parameters */
  @Nonnull
  private final List<Parameter> params;

  /**
   * Private constructor.
   * 
   * @param name Name of the template.
   */
  private TemplateBuilder(@Nonnull String name) {
    this.name = name;
    this.params = new ArrayList<>();
  }

  /**
   * Initialize a builder with the name of the template.
   * 
   * @param name Name of the template.
   * @return Builder initialized with the name of the template.
   */
  public static @Nonnull TemplateBuilder from(@Nullable String name) {
    TemplateBuilder builder = new TemplateBuilder(StringUtils.defaultIfEmpty(name, StringUtils.EMPTY));
    return builder;
  }

  /**
   * Initialize a builder with the template.
   * 
   * @param template Template.
   * @return Builder initialized with the template.
   */
  public static @Nonnull TemplateBuilder from(@Nonnull PageElementTemplate template) {
    TemplateBuilder builder = from(template.getTemplateName());
    for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
      PageElementTemplate.Parameter param = template.getParameter(paramNum);
      builder.addParam(param.getNameNotTrimmed(), param.getValueNotTrimmed());
    }
    return builder;
  }

  /**
   * Add a parameter to the template.
   * 
   * @param paramValue Value of the parameter.
   * @return Builder completed with the parameter.
   */
  public TemplateBuilder addParam(String paramValue) {
    return addParam(null, paramValue);
  }

  /**
   * Add a parameter to the template.
   * 
   * @param paramName Name of the parameter.
   * @param paramValue Value of the parameter.
   * @return Builder completed with the parameter.
   */
  public TemplateBuilder addParam(String paramName, String paramValue) {
    params.add(new Parameter(paramName, paramValue));
    return this;
  }

  /**
   * @return Textual representation of the title.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("{{");
    sb.append(name);
    for (Parameter param : params) {
      sb.append('|');
      if ((param.name != null) && (param.name.trim().length() > 0)) {
        sb.append(param.name);
        sb.append('=');
      }
      sb.append(StringUtils.defaultString(param.value, StringUtils.EMPTY));
    }
    sb.append("}}");
    return sb.toString();
  }

  /**
   * Bean for holding information about a parameter.
   */
  private static class Parameter {
    final String name;
    final String value;
    
    Parameter(String name, String value) {
      this.name = name;
      this.value = value;
    }
  }
}
