/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;

/**
 * Allow to group configuration for several templates.
 */
public class TemplateConfigurationGroup {

  @Nonnull
  private final Map<String, Set<String>> groups = new HashMap<>();

  /**
   * Add known groups from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for known groups.
   */
  public void addGroups(@Nullable List<String[]> rawConfiguration) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] line : rawConfiguration) {
      addGroup(line);
    }
  }

  /**
   * Add a known group from the full raw configuration.
   * 
   * @param rawConfiguration Raw configuration for a known group.
   */
  private void addGroup(@Nullable String[] rawConfiguration) {
    if ((rawConfiguration == null) || (rawConfiguration.length < 2)) {
      return;
    }
    String groupName = rawConfiguration[0];
    if (StringUtils.isEmpty(groupName)) {
      return;
    }
    Set<String> templates = groups.computeIfAbsent(groupName, k -> new HashSet<>());
    for (int templateNum = 1; templateNum < rawConfiguration.length; templateNum++) {
      String templateName = rawConfiguration[templateNum];
      if (!StringUtils.isEmpty(templateName)) {
        templates.add(templateName);
      }
    }
  }
  
  /**
   * Retrieve the set of corresponding template names.
   * 
   * @param templateNames Comma separated list of template or group names.
   * @return Set of corresponding template names.
   */
  @Nonnull
  public Set<String> getTemplateNames(@Nullable String templateNames) {
    if (StringUtils.isEmpty(templateNames) || templateNames.startsWith("#")) {
      return Collections.emptySet();
    }
    Set<String> result = new HashSet<>();
    String[] templates = templateNames.split(",");
    Set<String> addedGroups = new HashSet<>();
    for (String templateName : templates) {
      addTemplateName(result, templateName, addedGroups);
    }
    return result;
  }
  
  /**
   * Add a template/group name to a set of template names.
   * 
   * @param result Set of template names.
   * @param templateName Template/group name to add to the set.
   * @param addedGroups Already added groups (to prevent recursive loops).
   */
  private void addTemplateName(
      @Nonnull Set<String> result,
      @Nonnull String templateName,
      @Nonnull Set<String> addedGroups) {
    templateName = templateName.trim();
    if (StringUtils.isEmpty(templateName)) {
      return;
    }
    if (templateName.startsWith("+")) {
      String groupName = templateName.substring(1).trim();
      if (addedGroups.contains(groupName)) {
        return;
      }
      addedGroups.add(groupName);
      groups.getOrDefault(groupName, Collections.emptySet())
            .forEach(n -> addTemplateName(result, n, addedGroups));
    } else {
      result.add(templateName);
    }
  }
}
