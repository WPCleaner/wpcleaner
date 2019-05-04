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

  private static class ConfigurationVersion {

    /** All versions of configuration */
    private static Map<EnumWikipedia, ArrayList<ConfigurationVersion>> versions = new HashMap<>();

    /** WPCleaner configuration */
    private final WPCConfiguration config;

    /** Templates to be ignored */
    private final Map<String, List<Pair<String, String>>> ignoreTemplates;

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
  }
}
