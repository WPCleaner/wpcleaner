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

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.EnumWikipedia;

/**
 * Class to handle configuration about ISSN.
 */
public class PageElementISSNConfiguration {

  /** Analyzed configuration */
  private final ConfigurationVersion version;

  /**
   * Constructor.
   * 
   * @param config WPCleaner configuration.
   */
  public PageElementISSNConfiguration(WPCConfiguration config) {
    this.version = ConfigurationVersion.getConfigurationVersion(config);
  }

  /**
   * Tell if a template should be ignored for ISSN.
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
    Map<String, List<Pair<String, String>>> ignoreTemplates = version.getIgnoreTemplates();
    String templateName = Page.normalizeTitle(template.getTemplateName());
    List<Pair<String, String>> listParams = ignoreTemplates.get(templateName);
    if (listParams == null) {
      return false;
    }
    for (Pair<String, String> param : listParams) {
      if (param.getLeft() != null) {
        String paramValue = template.getParameterValue(param.getLeft());
        if (param.getRight() != null) {
          if ((paramValue != null) &&
              (paramValue.trim().equals(param.getRight()))) {
            return true; // Ignore all templates with this name and parameter set to a given value
          }
        } else {
          if (paramValue != null) {
            return true; // Ignore all templates with this name and parameter present
          }
        }
      } else {
        return true; // Ignore all templates with this name
      }
    }

    return false;
  }

  /**
   * Tell if a template automatically adds a missing dash.
   * 
   * @param templateName Template name.
   * @param paramName Parameter name.
   * @return True if the template automatically adds a missing dash for the given parameter. 
   */
  public boolean isAutoDashTemplate(String templateName, String paramName) {

    // Check parameters
    if ((templateName == null) || (paramName == null)) {
      return false;
    }

    // Check if the template automatically adds a missing dash
    Map<String, List<String>> autoDashTemplates = version.getAutoDashTemplates();
    List<String> listParams = autoDashTemplates.get(Page.normalizeTitle(templateName));
    if (listParams == null) {
      return false;
    }
    for (String param : listParams) {
      if (paramName.equals(param)) {
        return true;
      }
    }

    return isAutoFormatTemplate(templateName, paramName);
  }

  /**
   * Tell if a template automatically formats an ISSN.
   * 
   * @param templateName Template name.
   * @param paramName Parameter name.
   * @return True if the template automatically formats an ISSN for the given parameter. 
   */
  public boolean isAutoFormatTemplate(String templateName, String paramName) {

    // Check parameters
    if ((templateName == null) || (paramName == null)) {
      return false;
    }

    // Check if the template automatically formats an ISSN
    Map<String, List<String>> autoFormatTemplates = version.getAutoFormatTemplates();
    List<String> listParams = autoFormatTemplates.get(Page.normalizeTitle(templateName));
    if (listParams == null) {
      return false;
    }
    for (String param : listParams) {
      if (paramName.equals(param)) {
        return true;
      }
    }

    return false;
  }

  private static class ConfigurationVersion {

    /** All versions of configuration */
    private static Map<EnumWikipedia, ArrayList<ConfigurationVersion>> versions = new HashMap<>();

    /** WPCleaner configuration */
    private final WPCConfiguration config;

    /** Templates to be ignored */
    private final Map<String, List<Pair<String, String>>> ignoreTemplates;

    /** Templates which automatically add a missing dash to the ISSN */
    private final Map<String, List<String>> autoDashTemplates;

    /** Templates which automatically format the ISSN */
    private final Map<String, List<String>> autoFormatTemplates;

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
      this.autoDashTemplates = new HashMap<>();
      initAutoDashTemplates();
      this.autoFormatTemplates = new HashMap<>();
      initAutoFormatTemplates();
    }

    /**
     * Initialize configuration about templates to be ignored.
     */
    private void initIgnoreTemplates() {
      List<String[]> tmpIgnoreTemplates = config.getStringArrayList(WPCConfigurationStringList.ISSN_IGNORE_TEMPLATES);
      if (tmpIgnoreTemplates == null) {
        return;
      }
      for (String[] ignoreTemplate : tmpIgnoreTemplates) {
        if ((ignoreTemplate != null) && (ignoreTemplate.length > 0)) {
          String templateName = Page.normalizeTitle(ignoreTemplate[0]);
          String paramName = (ignoreTemplate.length > 1) ? ignoreTemplate[1].trim() : null;
          String paramValue = (ignoreTemplate.length > 2) ? ignoreTemplate[2].trim() : null;
          List<Pair<String, String>> listParams = ignoreTemplates.get(templateName);
          if (listParams == null) {
            listParams = new ArrayList<>();
            ignoreTemplates.put(templateName, listParams);
          }
          listParams.add(new ImmutablePair<String, String>(paramName, paramValue));
        }
      }
    }

    /**
     * @return Templates to be ignored.
     */
    public Map<String, List<Pair<String, String>>> getIgnoreTemplates() {
      return ignoreTemplates;
    }

    /**
     * Initialize configuration about templates that automatically add a missing dash.
     */
    private void initAutoDashTemplates() {
      List<String[]> tmpAutoDashTemplates = config.getStringArrayList(WPCConfigurationStringList.ISSN_AUTO_DASH_TEMPLATES);
      if (tmpAutoDashTemplates == null) {
        return;
      }
      for (String[] autoDashTemplate : tmpAutoDashTemplates) {
        if ((autoDashTemplate != null) &&
            (autoDashTemplate.length > 1) &&
            (autoDashTemplate[0] != null) &&
            (autoDashTemplate[1] != null)) {
          String name = Page.normalizeTitle(autoDashTemplate[0]);
          String param = autoDashTemplate[1].trim();
          List<String> listParams = autoDashTemplates.get(name);
          if (listParams == null) {
            listParams = new ArrayList<>();
            autoDashTemplates.put(name, listParams);
          }
          listParams.add(param);
        }
      }
    }

    /**
     * @return Templates that automatically add a missing dash.
     */
    public Map<String, List<String>> getAutoDashTemplates() {
      return autoDashTemplates;
    }

    /**
     * Initialize configuration about templates that automatically format an ISSN.
     */
    private void initAutoFormatTemplates() {
      List<String[]> tmpAutoFormatTemplates = config.getStringArrayList(WPCConfigurationStringList.ISSN_AUTO_FORMAT_TEMPLATES);
      if (tmpAutoFormatTemplates == null) {
        return;
      }
      for (String[] autoFormatTemplate : tmpAutoFormatTemplates) {
        if ((autoFormatTemplate != null) &&
            (autoFormatTemplate.length > 1) &&
            (autoFormatTemplate[0] != null) &&
            (autoFormatTemplate[1] != null)) {
          String name = Page.normalizeTitle(autoFormatTemplate[0]);
          String param = autoFormatTemplate[1].trim();
          List<String> listParams = autoFormatTemplates.get(name);
          if (listParams == null) {
            listParams = new ArrayList<>();
            autoFormatTemplates.put(name, listParams);
          }
          listParams.add(param);
        }
      }
    }

    /**
     * @return Templates that automatically format an ISSN.
     */
    public Map<String, List<String>> getAutoFormatTemplates() {
      return autoDashTemplates;
    }
  }
}
