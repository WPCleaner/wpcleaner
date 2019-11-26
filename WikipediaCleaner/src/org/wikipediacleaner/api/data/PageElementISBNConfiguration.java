/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;

/**
 * Class to handle configuration about ISBN.
 */
public class PageElementISBNConfiguration {

  /** Analyzed configuration */
  private final ConfigurationVersion version;

  /**
   * Constructor.
   * 
   * @param config WPCleaner configuration.
   */
  public PageElementISBNConfiguration(WPCConfiguration config) {
    this.version = ConfigurationVersion.getConfigurationVersion(config);
  }

  /**
   * Tell if a template should be ignored for ISBN.
   * 
   * @param template Template to be checked.
   * @return True if the template should be ignored.
   */
  public boolean shouldIgnoreTemplate(PageElementTemplate template) {

    // Check parameters
    if (template == null) {
      return false;
    }

    // Check if the template should be ignored
    Map<String, List<IgnoreTemplates>> ignoreTemplates = version.getIgnoreTemplates();
    String templateName = Page.normalizeTitle(template.getTemplateName());
    List<IgnoreTemplates> listParams = ignoreTemplates.get(templateName);
    if (listParams == null) {
      return false;
    }
    for (IgnoreTemplates param : listParams) {
      if (param.shouldIgnoreTemplate(template)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Tell if a template parameter should be ignored for ISBN.
   * 
   * @param template Template to be checked.
   * @param paramName Parameter name to be checked.
   * @return True if the template parameter should be ignored.
   */
  public boolean shouldIgnoreTemplateParam(PageElementTemplate template, String paramName) {

    // Check parameters
    if (template == null) {
      return false;
    }

    // Check if the template should be ignored
    Map<String, List<IgnoreTemplates>> ignoreTemplates = version.getIgnoreTemplates();
    String templateName = Page.normalizeTitle(template.getTemplateName());
    List<IgnoreTemplates> listParams = ignoreTemplates.get(templateName);
    if (listParams == null) {
      return false;
    }
    for (IgnoreTemplates param : listParams) {
      if (param.shouldIgnoreTemplateParam(template, paramName)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Manage configuration.
   */
  private static class ConfigurationVersion {

    /** All versions of configuration */
    private static Map<EnumWikipedia, ArrayList<ConfigurationVersion>> versions = new HashMap<>();

    /** WPCleaner configuration */
    private final WPCConfiguration config;

    /** Templates to be ignored */
    private final Map<String, List<IgnoreTemplates>> ignoreTemplates;

    /**
     * Retrieve configuration for a given version.
     * 
     * @param config Configuration.
     * @return
     */
    public static ConfigurationVersion getConfigurationVersion(WPCConfiguration config) {
      if ((config == null) || (config.getVersion() < 0)) {
        return null;
      }
      ConfigurationVersion version = null;
      synchronized (versions) {
        EnumWikipedia wiki = config.getWiki();
        ArrayList<ConfigurationVersion> listWiki = versions.get(wiki);
        if (listWiki == null) {
          listWiki = new ArrayList<>();
          versions.put(wiki, listWiki);
        }
        int versionNum = config.getVersion();
        while (listWiki.size() <= versionNum) {
          listWiki.add(null);
        }
        version = listWiki.get(versionNum);
        if (version == null) {
          version = new ConfigurationVersion(config);
          listWiki.set(versionNum, version);
        }
      }
      return version;
    }

    /**
     * @param config WPCleaner configuration.
     */
    private ConfigurationVersion(WPCConfiguration config) {
      this.config = config;
      this.ignoreTemplates = new HashMap<>();
      initIgnoreTemplates();
    }

    /**
     * Initialize configuration about templates to be ignored.
     */
    private void initIgnoreTemplates() {
      List<String[]> tmpIgnoreTemplates = config.getStringArrayList(WPCConfigurationStringList.ISBN_IGNORE_TEMPLATES);
      if (tmpIgnoreTemplates == null) {
        return;
      }
      for (String[] ignoreTemplate : tmpIgnoreTemplates) {
        if ((ignoreTemplate != null) && (ignoreTemplate.length > 0)) {
          String templateName = Page.normalizeTitle(ignoreTemplate[0]);
          String paramName = (ignoreTemplate.length > 1) ? ignoreTemplate[1].trim() : null;
          String paramValue = (ignoreTemplate.length > 2) ? ignoreTemplate[2].trim() : null;
          String ignoredParam = (ignoreTemplate.length > 3) ? ignoreTemplate[3].trim() : null;
          List<IgnoreTemplates> listParams = ignoreTemplates.get(templateName);
          if (listParams == null) {
            listParams = new ArrayList<>();
            ignoreTemplates.put(templateName, listParams);
          }
          listParams.add(new IgnoreTemplates(paramName, paramValue, ignoredParam));
        }
      }
    }

    /**
     * @return Templates to be ignored.
     */
    public Map<String, List<IgnoreTemplates>> getIgnoreTemplates() {
      return ignoreTemplates;
    }
  }

  /**
   * Handle templates to be ignored.
   */
  private static class IgnoreTemplates {

    /** Parameter name for ignoring template */
    private final String paramName;

    /** Parameter value for ignoring template */
    private final String paramValue;

    /** Parameter ignored if the template is not entirely ignored */
    private final String ignoredParam;

    /**
     * @param paramName Parameter name for ignoring template.
     * @param paramValue Parameter value for ignoring template.
     */
    public IgnoreTemplates(
        String paramName,
        String paramValue,
        String ignoredParam) {
      this.paramName = paramName;
      this.paramValue = paramValue;
      this.ignoredParam = StringUtils.isNotEmpty(ignoredParam) ? ignoredParam : null;
    }

    /**
     * @param template Template to analyze.
     * @return True if the template should be entirely ignored.
     */
    public boolean shouldIgnoreTemplate(PageElementTemplate template) {
      // Do not ignore the template if only a parameter should be ignored
      if (ignoredParam != null) {
        return false;
      }

      // Ignore all templates with this name if no parameter name is given
      if (paramName == null) {
        return true;
      }

      // Ignore template based on the parameter value
      String value = template.getParameterValue(paramName);
      if (paramValue != null) {
        // Ignore all templates with this name and parameter set to a given value
        if ((value != null) && (value.trim().equals(paramValue))) {
          return true;
        }
      } else {
        // Ignore all templates with this name and parameter present
        if (value != null) {
          return true;
        }
      }

      return false;
    }

    /**
     * @param template Template to analyze.
     * @param name Parameter name to check
     * @return True if the template should be entirely ignored.
     */
    public boolean shouldIgnoreTemplateParam(PageElementTemplate template, String name) {
      // If the ignored parameter is not set, check only the template
      if (ignoredParam == null) {
        return shouldIgnoreTemplate(template);
      }

      // Check the the ignored parameter is the one to check
      if (!ignoredParam.equals(name)) {
        return false;
      }

      // Ignore all templates with this name if no parameter name is given
      if (paramName == null) {
        return true;
      }

      // Ignore template based on the parameter value
      String value = template.getParameterValue(paramName);
      if (paramValue != null) {
        // Ignore all templates with this name and parameter set to a given value
        if ((value != null) && (value.trim().equals(paramValue))) {
          return true;
        }
      } else {
        // Ignore all templates with this name and parameter present
        if (value != null) {
          return true;
        }
      }

      return false;
    }
  }
}
