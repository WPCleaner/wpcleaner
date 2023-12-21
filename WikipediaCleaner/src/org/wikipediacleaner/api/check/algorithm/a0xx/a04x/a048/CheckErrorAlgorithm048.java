/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a04x.a048;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;
import org.wikipediacleaner.api.data.contents.magicword.FunctionMagicWordType;
import org.wikipediacleaner.api.data.contents.magicword.MagicWord;
import org.wikipediacleaner.api.data.contents.magicword.MagicWordType;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 48 of check wikipedia project.
 * Error 48: Title linked in text
 */
public class CheckErrorAlgorithm048 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Remove all links to title (first in bold)"),
    GT._T("Remove all links to title"),
  };

  public CheckErrorAlgorithm048() {
    super("Title linked in text");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Do not report redirects
    if (analysis.getPage().getRedirects().isRedirect()) {
      return false;
    }
    Integer namespace = analysis.getPage().getNamespace();
    if ((namespace != null) && (namespace.intValue() == Namespace.USER_TALK)) {
      return false;
    }

    // Analyze each internal link
    boolean result = false;
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    String pageTitle = Page.normalizeTitle(analysis.getPage().getTitle());
    for (PageElementInternalLink link : links) {
      result |= analyzeInternalLink(link, pageTitle, analysis, errors);
    }

    return result;
  }

  private static final Set<MagicWordType> EXCLUDE_MAGIC_WORDS = Stream
      .of(
          FunctionMagicWordType.LST,
          FunctionMagicWordType.LST_H,
          FunctionMagicWordType.LST_X,
          FunctionMagicWordType.SECTION,
          FunctionMagicWordType.SECTION_H,
          FunctionMagicWordType.SECTION_X)
      .collect(Collectors.toSet());
  private static final Set<TagType> EXCLUDE_TAGS = Stream
      .of(WikiTagType.SECTION)
      .collect(Collectors.toSet());

  /**
   * Analyze an internal link to check if errors are present.
   * 
   * @param link Internal link to be checked.
   * @param pageTitle Page title.
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLink(
      PageElementInternalLink link, String pageTitle,
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {

    // Check if there's a potential error
    boolean errorFoundFull = Page.areSameTitle(pageTitle, true, link.getFullLink(), false);
    boolean errorFoundAnchor = !errorFoundFull && Page.areSameTitle(pageTitle, true, link.getLink(), false);
    if (!errorFoundFull && !errorFoundAnchor) {
      return false;
    }
    if (errorFoundAnchor) {
      Integer namespace = analysis.getPage().getNamespace();
      if ((namespace == null) || (Namespace.MAIN != namespace)) {
        return false;
      }
    }

    // Ignore if the link is inside some tags
    if ((analysis.getSurroundingTag(WikiTagType.INCLUDEONLY, link.getBeginIndex()) != null) ||
        (analysis.getSurroundingTag(WikiTagType.MAPFRAME, link.getBeginIndex()) != null) ||
        (analysis.getSurroundingTag(WikiTagType.NOWIKI, link.getBeginIndex()) != null) ||
        (analysis.getSurroundingTag(WikiTagType.ONLYINCLUDE, link.getBeginIndex()) != null) ||
        (analysis.getSurroundingTag(WikiTagType.TIMELINE, link.getBeginIndex()) != null)) {
      return false;
    }

    // Ignore if the link is in an image map tag
    PageElementTag tagImageMap = analysis.getSurroundingTag(WikiTagType.IMAGEMAP, link.getBeginIndex()); 
    if (!imagemap && (tagImageMap != null)) {
      return false;
    }

    // Ignore for some special cases
    List<PageElementFunction> functions = analysis.getFunctions();
    for (PageElementFunction function : functions) {
      MagicWord magicWord = function.getMagicWord();
      if ((magicWord != null) && EXCLUDE_MAGIC_WORDS.contains(magicWord.getType())) {
        return false;
      }
    }
    for (TagType tagType : EXCLUDE_TAGS) {
      List<PageElementTag> tags =analysis.getTags(tagType);
      if ((tags != null) && !tags.isEmpty()) {
        return false;
      }
    }

    // Ignore if the link is inside some templates
    if (!ignoredTemplates.isEmpty()) {
      PageElementTemplate template = analysis.isInTemplate(link.getBeginIndex());
      if ((template != null) &&
          ignoredTemplates.contains(Page.normalizeTitle(template.getTemplateName()))) {
        return false;
      }
    }

    // Report error
    if (errors == null) {
      return true;
    }
    String contents = analysis.getContents();
    int beginIndex = link.getBeginIndex();
    int endIndex = link.getEndIndex();

    // Report in image map tag
    if (tagImageMap != null) {
      int previousCR = ContentsUtil.getLineBeginIndex(contents, beginIndex);
      int nextCR = ContentsUtil.getLineEndIndex(contents, endIndex);
      nextCR = Math.min(nextCR, tagImageMap.getMatchingTag().getBeginIndex());
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, previousCR, nextCR);
      if ((previousCR > tagImageMap.getEndIndex()) &&
          (contents.charAt(nextCR) == '\n')) {
        errorResult.addReplacement("", GT._T("Delete"));
      }
      errors.add(errorResult);
      return true;
    }

    // Analysis regarding titles
    boolean beforeFirstTitle = true;
    List<PageElementTitle> titles = analysis.getTitles();
    if ((titles != null) && !titles.isEmpty()) {
      PageElementTitle title = titles.get(0);
      if (title.getBeginIndex() < endIndex) {
        beforeFirstTitle = false;
      }
    }

    // Report anchored tag
    if (errorFoundAnchor) {
      CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
      String anchor = link.getAnchor();
      if ((anchor != null) &&
          (anchor.trim().length() > 0)) {
        boolean automatic = true;
        if (anchor.trim().startsWith("cite") ||
            anchor.trim().startsWith("fn") ||
            anchor.trim().startsWith("ftn") ||
            anchor.trim().startsWith("sdfootnote")) {
          automatic = false;
        }
        if ((link.getText() == null) ||
            (link.getText().trim().length() == 0)) {
          automatic = false;
          String replacement = InternalLinkBuilder.from(null)
              .withAnchor(anchor)
              .withText(anchor)
              .toString();
          errorResult.addReplacement(replacement);
        }
        String replacement = InternalLinkBuilder.from(null)
            .withAnchor(anchor)
            .withText(link.getDisplayedTextNotTrimmed())
            .toString();
        errorResult.addReplacement(replacement, automatic);
      }
      errors.add(errorResult);
      return true;
    }

    // Analysis regarding bold
    boolean inBold = true;
    if ((beginIndex < 3) || !contents.startsWith("'''", beginIndex - 3)) {
      inBold = false;
    } else if ((beginIndex > 3) && (contents.charAt(beginIndex - 4) == '\'')) {
      inBold = false;
    }
    if (!contents.startsWith("'''", endIndex)) {
      inBold = false;
    } else if ((contents.length() > endIndex + 4) && (contents.charAt(endIndex + 4) == '\'')) {
      inBold = false;
    }

    // Apostrophe before
    String prefix = "";
    boolean apostropheBefore = false;
    if ((beginIndex > 1) &&
        (contents.charAt(beginIndex - 1) == '\'') &&
        (contents.charAt(beginIndex - 2) != '\'')) {
      apostropheBefore = true;
      prefix = "'";
      beginIndex--;
    }

    // Suggestions
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex);
    errorResult.addReplacement(prefix + link.getDisplayedText(), !beforeFirstTitle || inBold);
    if (!inBold) {
      if (apostropheBefore) {
        String apostropheTemplate = analysis.getWPCConfiguration().getString(
            WPCConfigurationString.APOSTROPHE_TEMPLATE);
        if (apostropheTemplate != null) {
          errorResult.addReplacement(
              TemplateBuilder.from(apostropheTemplate).toString() +
              "'''" + link.getDisplayedText() + "'''");
        }
      }
      errorResult.addReplacement(prefix + "'''" + link.getDisplayedText() + "'''");
    }
    errors.add(errorResult);

    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {

    // Find first title
    String contents = analysis.getContents();
    int firstTitle = 0;
    if (fixName.equals(globalFixes[0])) {
      Collection<PageElementTitle> titles = analysis.getTitles();
      if ((titles != null) && (titles.size() > 0)) {
        firstTitle = titles.iterator().next().getBeginIndex();
      } else {
        firstTitle = contents.length();
      }
    }

    // Replace all texts
    StringBuilder newContents = new StringBuilder(contents.length());
    String pageTitle = analysis.getPage().getTitle();
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    int currentIndex = 0;
    for (PageElementInternalLink link : links) {
      if (Page.areSameTitle(pageTitle, link.getFullLink())) {
        PageElementTag tagImagemap = analysis.getSurroundingTag(
            WikiTagType.IMAGEMAP, link.getBeginIndex());
        if (tagImagemap != null) {
          int previousCR = ContentsUtil.getLineBeginIndex(contents, link.getBeginIndex());
          int nextCR = ContentsUtil.getLineEndIndex(contents, link.getEndIndex());
          nextCR = Math.min(nextCR, tagImagemap.getMatchingTag().getBeginIndex());
          if ((previousCR > tagImagemap.getEndIndex()) &&
              (contents.charAt(nextCR) == '\n')) {
            if (previousCR > currentIndex) {
              newContents.append(contents.substring(currentIndex, previousCR));
              currentIndex = nextCR;
            }
          }
        } else {
          if (link.getBeginIndex() > currentIndex) {
            newContents.append(contents.substring(currentIndex, link.getBeginIndex()));
          }
          if ((currentIndex == 0) && (link.getBeginIndex() < firstTitle)) {
            newContents.append("'''");
            newContents.append(link.getDisplayedText());
            newContents.append("'''");
          } else {
            newContents.append(link.getDisplayedText());
          }
          currentIndex = link.getEndIndex();
        }
      }
    }
    if (currentIndex < contents.length()) {
      newContents.append(contents.substring(currentIndex));
    }
    return newContents.toString();
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates to ignore */
  private static final String PARAMETER_IGNORE_TEMPLATES = "ignore_templates";

  /** Flag to report also in image maps */
  private static final String PARAMETER_IMAGEMAP = "imagemap";

  /** Template groups */
  private static final String PARAMETER_TEMPLATE_GROUPS = "template_groups";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    imagemap = Boolean.parseBoolean(
        getSpecificProperty(PARAMETER_IMAGEMAP, true, true, false));

    String tmp = getSpecificProperty(PARAMETER_TEMPLATE_GROUPS, true, true, false);
    TemplateConfigurationGroup group = new TemplateConfigurationGroup();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      group.addGroups(tmpList);
    }
    List<String[]> generalList = getWPCConfiguration().getStringArrayList(WPCConfigurationStringList.TEMPLATE_GROUPS);
    if (generalList != null) {
      group.addGroups(generalList);
    }

    tmp = getSpecificProperty(PARAMETER_IGNORE_TEMPLATES, true, true, false);
    ignoredTemplates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      for (String[] line : tmpList) {
        if (line.length > 0) {
          for (String template : group.getTemplateNames(line[0])) {
            ignoredTemplates.add(template);
          }
        }
      }
    }
  }

  /** Flag to report also in image maps */
  private boolean imagemap = false;

  private Set<String> ignoredTemplates = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_IGNORE_TEMPLATES,
        GT._T("List of templates in which error should be ignored"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Name of the template in which error should be ignored"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_IMAGEMAP,
        GT._T("Set to true to report also links in <imagemap>"),
        new AlgorithmParameterElement(
            "true/false",
            GT._T("Set to true to report also links in <imagemap>"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATE_GROUPS,
        GT._T("Groups of templates"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "group",
                GT._T("Name of the group")),
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of a template in the group"),
                false,
                true)
        },
        true));
  }
}
