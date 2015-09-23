/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.menu;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractButton;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.text.Element;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.LinkReplacement;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.TemplateMatcher;
import org.wikipediacleaner.gui.swing.action.ChangePreferredDisambiguationAction;
import org.wikipediacleaner.gui.swing.action.MarkLinkAction;
import org.wikipediacleaner.gui.swing.action.ReloadCategoryMembersAction;
import org.wikipediacleaner.gui.swing.action.ReplaceLinkAction;
import org.wikipediacleaner.gui.swing.action.ReplaceTextAction;
import org.wikipediacleaner.gui.swing.action.RevertLinkAction;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.StringCheckerUnauthorizedCharacters;


/**
 * A helper class to manage contextual menu.
 */
public class MWPaneDisambiguationMenuCreator extends BasicMenuCreator {

  /**
   * Add submenus for marking link as normal.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   * @param text Text.
   * @param element Element.
   * @param textPane Text pane.
   */
  public void addMarkAsNormal(
      EnumWikipedia wiki, JPopupMenu popup, Page page, String text,
      Element element, JTextPane textPane) {
    if ((text != null) &&
        (page != null) &&
        Boolean.TRUE.equals(page.isDisambiguationPage()) &&
        (wiki != null)) {

      // Add a menu for marking as normal link using a template
      List<String[]> templates = wiki.getConfiguration().getStringArrayList(
          WPCConfigurationStringList.TEMPLATES_FOR_DAB_LINK);
      if ((templates != null) && !templates.isEmpty()) {
        if (templates.size() > 1) {
          JMenu submenu = new JMenu(GT._("Mark as normal link"));
          for (String[] template : templates) {
            String replacement = createTextForTemplate(template[0], page.getTitle(), text);
            String message = GT._("Using {0}", "{{" + template[0] + "}}");
            if ((template.length > 1) && (template[1].trim().length() > 0)) {
              message += " - " + template[1].trim();
            }
            addItem(
                submenu, null, message, true,
                new ReplaceTextAction(page, replacement, element, textPane));
          }
          popup.add(submenu);
        } else {
          String replacement = createTextForTemplate(templates.get(0)[0], page.getTitle(), text);
          addItem(
              popup, null, GT._("Mark as normal link using template"), true,
              new ReplaceTextAction(page, replacement, element, textPane));
        }
      }

      // Add a menu for marking as normal links using a comment
      List<String> comments = wiki.getConfiguration().getStringList(
          WPCConfigurationStringList.COMMENTS_FOR_DAB_LINK);
      if ((comments != null) && !comments.isEmpty()) {
        if (comments.size() > 1) {
          JMenu submenu = new JMenu(GT._("Mark as normal link"));
          for (String comment : comments) {
            String replacement =
                PageElementInternalLink.createInternalLink(page.getTitle(), text) +
                PageElementComment.createComment(comment);
            addItem(
                submenu, null, GT._("Using {0}", comment), true,
                new ReplaceTextAction(page, replacement, element, textPane));
          }
          popup.add(submenu);
        } else {
          String replacement =
              PageElementInternalLink.createInternalLink(page.getTitle(), text) +
              PageElementComment.createComment(comments.get(0));
          addItem(
              popup, null, GT._("Mark as normal link using comment"), true,
              new ReplaceTextAction(page, replacement, element, textPane));
        }
      }
    }
  }

  /**
   * Add submenus for marking link as requiring help to disambiguate.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   * @param text Text.
   * @param element Element.
   * @param textPane Text pane.
   * @param checkBox Check Box.
   */
  public void addMarkAsNeedingHelp(
      EnumWikipedia wiki, JPopupMenu popup, Page page, String text,
      Element element, JTextPane textPane, AbstractButton checkBox) {
    List<String> templates = null;
    List<List<String>> templatesAfter = null;
    int templatesCount = 0;
    if (wiki != null) {
      WPCConfiguration config = wiki.getConfiguration();
      templates = config.getStringList(
          WPCConfigurationStringList.TEMPLATES_FOR_NEEDING_HELP);
      if (templates != null) {
        templatesCount += templates.size();
      }
      templatesAfter = config.getTemplatesAfterAskHelp();
      if (templatesAfter != null) {
        templatesCount += templatesAfter.size();
      }
    }
    if ((text != null) && (page != null) &&
        Boolean.TRUE.equals(page.isDisambiguationPage()) &&
        (templatesCount > 0)) {
      if (templatesCount > 1) {
        JMenu submenu = new JMenu(GT._("Mark as needing help"));
        if (templates != null) {
          for (String template : templates) {
            addItem(
                submenu, null, GT._("Using {0}", "{{" + template + "}}"), true,
                new MarkLinkAction(
                    page, element,
                    createTextForTemplate(template, page.getTitle(), text),
                    textPane, checkBox));
          }
        }
        if (templatesAfter != null) {
          for (List<String> template : templatesAfter) {
            String templateName = template.get(0);
            addItem(
                submenu, null, GT._("Using {0}", "[[…]]{{" + templateName + "}}"), true,
                new MarkLinkAction(
                    page, element,
                    createTextForTemplateAfterLink(template, page.getTitle(), text),
                    textPane, checkBox));
          }
        }
        popup.add(submenu);
      } else {
        String newText = null;
        if ((templates != null) && (templates.size() > 0)) {
          newText = createTextForTemplate(templates.get(0), page.getTitle(), text);
        } else if ((templatesAfter != null) && (templatesAfter.size() > 0)) {
          newText = createTextForTemplateAfterLink(templatesAfter.get(0), page.getTitle(), text);
        }
        if (newText != null) {
          addItem(
              popup, null, GT._("Mark as needing help"), true,
              new MarkLinkAction(page, element, newText.toString(), textPane, checkBox));
        }
      }
    }
  }

  /**
   * Add submenus for linking texts.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   * @param text Text.
   * @param element Element.
   * @param textPane Text pane.
   */
  public void addLinkText(
      EnumWikipedia wiki, JPopupMenu popup, Page page, String text,
      Element element, JTextPane textPane) {
    List<String> templates = null;
    if (wiki != null) {
      templates = wiki.getConfiguration().getStringList(
          WPCConfigurationStringList.TEMPLATES_FOR_LINKING_TEXT);
    }
    if ((text != null) &&
        (page != null) &&
        Boolean.TRUE.equals(page.isDisambiguationPage()) &&
        (templates != null) &&
        (templates.size() > 0)) {
      if (templates.size() > 1) {
        JMenu submenu = new JMenu(GT._("Link text"));
        for (String template : templates) {
          addItem(
              submenu, null, GT._("Using {0}", "{{" + template + "}}"), true,
              new MarkLinkAction(
                  page, element,
                  createTextForTemplate(template, page.getTitle(), text),
                  textPane, null));
        }
        popup.add(submenu);
      } else {
        addItem(
            popup, null, GT._("Link text"), true,
            new MarkLinkAction(
                page, element,
                createTextForTemplate(templates.get(0), page.getTitle(), text),
                textPane, null));
      }
    }
  }

  /**
   * Add submenus for replacing links.
   * 
   * @param wiki Wiki.
   * @param popup Popup menu.
   * @param page Page.
   * @param text Text.
   * @param element Element.
   * @param textPane Text pane.
   */
  public void addReplaceLink(
      EnumWikipedia wiki, JPopupMenu popup, Page page, String text,
      Element element, JTextPane textPane) {
    if (text == null) {
      return;
    }

    // Disambiguation page
    if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
      Map<Page, List<String>> anchorsMap = new HashMap<Page, List<String>>();
      List<Page> links = page.getLinksWithRedirect(anchorsMap);
      JMenu submenuLink = new JMenu(GT._("Link to"));
      JMenu submenuReplace = new JMenu(GT._("Replace with"));
      JMenu submenuAddPreferred = new JMenu(GT._("Add to preferred disambiguations"));
      KeyStroke lastLinkKeyStroke = MWPane.getLastLinkKeyStroke();
      KeyStroke lastReplaceKeyStroke = MWPane.getLastReplaceKeyStroke();

      // Determine if separators are needed
      List<String> wiktionary = page.getWiktionaryLinks();
      boolean separators = false;
      if (((links != null) && (links.size() > 0)) ||
          ((wiktionary != null) && (wiktionary.size() > 0))) {
        separators = true;
      }

      // Retrieve preferred disambiguations
      Configuration config = Configuration.getConfiguration();
      List<String> preferredDabs = config.getStringSubList(
          page.getWikipedia(), Configuration.SUB_ARRAY_PREFERRED_DAB, page.getTitle());

      // Last replacement
      int fixedBeginLink = 0;
      int fixedEndLink = 0;
      int fixedBeginReplace = 0;
      int fixedEndReplace = 0;
      String withLastSuffix = LinkReplacement.getPossibleLastSuffix(page.getTitle(), links);
      String last = LinkReplacement.getLastReplacement(page.getTitle());
      if (withLastSuffix != null) {
        preferredDabs.remove(withLastSuffix);
        addItem(
            popup, null, GT._("&Link to {0}", withLastSuffix), false,
            new ReplaceLinkAction(page.getTitle(), withLastSuffix, text, element, textPane, false),
            lastLinkKeyStroke);
        lastLinkKeyStroke = null;
        addItem(
            popup, null, GT._("&Replace with {0}", withLastSuffix), false,
            new ReplaceLinkAction(page.getTitle(), withLastSuffix, text, element, textPane, true),
            lastReplaceKeyStroke);
        lastReplaceKeyStroke = null;
      } else {
        if ((last != null) && (!last.equals(withLastSuffix))) {
          preferredDabs.remove(last);
          addItem(
              popup, null, GT._("&Link to {0}", last), false,
              new ReplaceLinkAction(page.getTitle(), last, text, element, textPane, false),
              lastLinkKeyStroke);
          lastLinkKeyStroke = null;
          addItem(
              popup, null, GT._("&Replace with {0}", last), false,
              new ReplaceLinkAction(page.getTitle(), last, text, element, textPane, true),
              lastReplaceKeyStroke);
          lastReplaceKeyStroke = null;
        }
      }
      if (!preferredDabs.isEmpty()) {
        for (String title : preferredDabs) {
          fixedBeginLink += addItem(
              submenuLink, null, title, true,
              new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, false),
              lastLinkKeyStroke);
          lastLinkKeyStroke = null;
          fixedBeginReplace += addItem(
              submenuReplace, null, title, true,
              new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, true),
              lastReplaceKeyStroke);
          lastReplaceKeyStroke = null;
        }
      }

      // Separators
      if (separators) {
        fixedBeginLink += addSeparator(submenuLink);
        fixedBeginReplace += addSeparator(submenuReplace);
      }

      // Wiktionary links
      if ((wiktionary != null) && (wiktionary.size() > 0)) {
        for (String wikt : wiktionary) {
          String name = "wikt:" + wikt;
          addItem(
              submenuLink, null, name, true,
              new ReplaceLinkAction(page.getTitle(), ":" + name, text, element, textPane, false));
        }
      }

      // Possible links
      if ((links != null) && (links.size() > 0)) {
        for (Page p : links) {
          if (p.isRedirect()) {
            JMenu submenu1 = new JMenu(p.getTitle());
            JMenu submenu2 = new JMenu(p.getTitle());
            JMenu submenu3 = new JMenu(p.getTitle());
            Map<Page, List<String>> anchorsRedirectMap = new HashMap<Page, List<String>>();
            p.getLinksWithRedirect(anchorsRedirectMap);
            
            Iterator<Page> iter = p.getRedirectIteratorWithPage();
            while (iter.hasNext()) {
              Page pageTmp = iter.next();
              List<String> anchors = anchorsRedirectMap.get(pageTmp);

              addItem(
                  submenu1, pageTmp, null, true,
                  new ReplaceLinkAction(page.getTitle(), pageTmp.getTitle(), text, element, textPane, false));

              addItem(
                  submenu2, pageTmp, null, true,
                  new ReplaceLinkAction(page.getTitle(), pageTmp.getTitle(), text, element, textPane, true));

              addItem(
                  submenu3, pageTmp, null, true,
                  new ChangePreferredDisambiguationAction(
                      page.getWikipedia(), page.getTitle(), pageTmp.getTitle(), true));

              if ((anchors != null) && (anchors.size() > 0)) {
                for (String anchor : anchors) {
                  addItem(
                      submenu1, pageTmp, anchor, true,
                      new ReplaceLinkAction(page.getTitle(), anchor, text, element, textPane, false));

                  addItem(
                      submenu2, pageTmp, anchor, true,
                      new ReplaceLinkAction(page.getTitle(), anchor, text, element, textPane, true));

                  if (!preferredDabs.contains(anchor)) {
                    addItem(
                        submenu3, pageTmp, anchor, true,
                        new ChangePreferredDisambiguationAction(
                            page.getWikipedia(), page.getTitle(), anchor, true));
                  }
                }
              }
            }
            
            submenuLink.add(submenu1);
            submenuReplace.add(submenu2);
            submenuAddPreferred.add(submenu3);
          } else if ((p.getNamespace() != null) &&
                     (p.getNamespace().intValue() == Namespace.CATEGORY)) {
            JMenu submenu1 = new JMenu(p.getTitle());
            JMenu submenu2 = new JMenu(p.getTitle());

            addItem(
                submenu1, p, null, true,
                new ReplaceLinkAction(page.getTitle(), p.getTitle(), text, element, textPane, false));
            addItem(
                submenu2, p, null, true,
                new ReplaceLinkAction(page.getTitle(), p.getTitle(), text, element, textPane, true));

            addSeparator(submenu1);
            addSeparator(submenu2);

            List<Page> categoryMembers = p.getRelatedPages(Page.RelatedPages.CATEGORY_MEMBERS);
            if (categoryMembers == null) {
              addItem(
                  submenu1, null, GT._("Reload category members"), true,
                  new ReloadCategoryMembersAction(wiki, p, null));
            } else {
              for (Page p2 : categoryMembers) {
                boolean already = false;
                for (Page p3 : links) {
                  if (Page.areSameTitle(p2.getTitle(), p3.getTitle())) {
                    already = true;
                  }
                }
                if (!already) {
                  addItem(
                      submenu1, p2, null, true,
                      new ReplaceLinkAction(page.getTitle(), p2.getTitle(), text, element, textPane, false));
                  addItem(
                      submenu2, p2, null, true,
                      new ReplaceLinkAction(page.getTitle(), p2.getTitle(), text, element, textPane, true));
                }
              }
            }

            addSubmenu(submenuLink, submenu1, 2, 0);
            addSubmenu(submenuReplace, submenu2, 2, 0);
          } else {
            addItem(
                submenuLink, p, null, true,
                new ReplaceLinkAction(page.getTitle(), p.getTitle(), text, element, textPane, false));

            addItem(
                submenuReplace, p, null, true,
                new ReplaceLinkAction(page.getTitle(), p.getTitle(), text, element, textPane, true));

            if (!preferredDabs.contains(p.getTitle())) {
              addItem(
                  submenuAddPreferred, p, null, true,
                  new ChangePreferredDisambiguationAction(
                      page.getWikipedia(), page.getTitle(), p.getTitle(), true));
            }

            // Anchors
            List<String> anchors = anchorsMap.get(p);
            if (anchors != null) {
              for (String anchor : anchors) {
                addItem(
                    submenuLink, p, anchor, true,
                    new ReplaceLinkAction(page.getTitle(), anchor, text, element, textPane, false));

                addItem(
                    submenuReplace, p, anchor, true,
                    new ReplaceLinkAction(page.getTitle(), anchor, text, element, textPane, true));

                if (!preferredDabs.contains(anchor)) {
                  addItem(
                      submenuAddPreferred, p, anchor, true,
                      new ChangePreferredDisambiguationAction(
                          page.getWikipedia(), page.getTitle(), anchor, true));
                }
              }
            }
          }
        }
      }

      // Last replacement
      if (separators) {
        if (!preferredDabs.isEmpty()) {
          fixedEndLink += addSeparator(submenuLink);
          fixedEndReplace += addSeparator(submenuReplace);
          for (String title : preferredDabs) {
            if ((withLastSuffix != null) && (withLastSuffix.equals(title))) {
              withLastSuffix = null;
            }
            fixedEndLink += addItem(
                submenuLink, null, title, true,
                new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, false),
                (preferredDabs.size() == 1) ? lastLinkKeyStroke : null);
            fixedEndReplace += addItem(
                submenuReplace, null, title, true,
                new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, true),
                (preferredDabs.size() == 1) ? lastReplaceKeyStroke : null);
          }
        }
      }

      addSubmenu(popup, submenuLink, fixedBeginLink, fixedEndLink);
      addSubmenu(popup, submenuReplace, fixedBeginReplace, fixedEndReplace);

      submenuAddPreferred.addSeparator();
      addItem(
          submenuAddPreferred, null, GT._("Add other preferred disambiguation..."), true,
          new ChangePreferredDisambiguationAction(
              page.getWikipedia(),
              page.getTitle(), textPane,
              GT._("What link do you want to add to the preferred disambiguations?"),
              null,
              new StringCheckerUnauthorizedCharacters("[|]")),
          null);
      addSubmenu(popup, submenuAddPreferred, 0, 2);

      if (!preferredDabs.isEmpty()) {
        JMenu submenuRemove = new JMenu(GT._("Remove from preferred disambiguations"));
        for (String title : preferredDabs) {
          addItem(
              submenuRemove, null, title, true,
              new ChangePreferredDisambiguationAction(
                  page.getWikipedia(), page.getTitle(), title, false),
              null);
        }
        addSubmenu(popup, submenuRemove, 0, 0);
      }

      if (!Page.areSameTitle(text, page.getTitle())) {
        String newLink = PageElementInternalLink.createInternalLink(text, page.getTitle());
        addItem(
            popup, null, GT._("Reverse to {0}", newLink), true,
            new RevertLinkAction(page.getTitle(), text, element, textPane), null);
      }

    // Redirect page
    } else if (page.isRedirect()) {
      String title = page.getRedirectTitle();

      addItem(
          popup, null, GT._("&Link to {0}", title), false,
          new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, false));

      addItem(
          popup, null, GT._("&Replace with {0}", title), false,
          new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, true));

      if (!Page.areSameTitle(text, page.getTitle())) {
        String newLink = PageElementInternalLink.createInternalLink(text, page.getTitle());
        addItem(
            popup, null, GT._("Reverse to {0}", newLink), true,
            new RevertLinkAction(page.getTitle(), text, element, textPane));
      }

    // Missing page
    } else if (Boolean.FALSE.equals(page.isExisting())) {
      if (!Page.areSameTitle(text, page.getTitle())) {
        String newLink = PageElementInternalLink.createInternalLink(text, page.getTitle());
        addItem(
            popup, null, GT._("Reverse to {0}", newLink), true,
            new RevertLinkAction(page.getTitle(), text, element, textPane));
      }
    }
  }

  /**
   * Add sub menus for replacing templates.
   * 
   * @param wiki Wiki.
   * @param popup Pop up menu.
   * @param template Template.
   * @param matcher Template matcher.
   * @param disambigPage Disambiguation page.
   * @param page Page containing the text.
   * @param element Element.
   * @param textPane Text pane.
   */
  public void addReplaceTemplate(
      EnumWikipedia wiki, JPopupMenu popup,
      PageElementTemplate template, TemplateMatcher matcher,
      Page disambigPage, Page page, Element element, JTextPane textPane) {
    if ((wiki == null) ||
        (popup == null) ||
        (template == null) ||
        (matcher == null) ||
        (disambigPage == null)) {
      return;
    }
    if (!Boolean.TRUE.equals(disambigPage.isDisambiguationPage())) {
      return;
    }

    // Retrieve possible replacements
    List<String> replacements = matcher.getReplacements(disambigPage, template);
    if ((replacements == null) || (replacements.isEmpty())) {
      return;
    }

    // Retrieve preferred disambiguations
    Configuration config = Configuration.getConfiguration();
    List<String> preferredDabs = config.getStringSubList(
        disambigPage.getWikipedia(),
        Configuration.SUB_ARRAY_PREFERRED_DAB,
        disambigPage.getTitle());

    // Retrieve various information
    List<String> wiktionary = disambigPage.getWiktionaryLinks();
    Map<Page, List<String>> anchorsMap = new HashMap<Page, List<String>>();
    List<Page> links = disambigPage.getLinksWithRedirect(anchorsMap);

    // Checking all possible replacements
    for (int indexReplacement = 0; indexReplacement < replacements.size(); indexReplacement++) {
      String replacement = replacements.get(indexReplacement);
      if (replacement != null) {
        JMenu submenu = new JMenu(replacement);

        int fixedBegin = 0;
        int fixedEnd = 0;
        boolean separators = false;

        if (!preferredDabs.isEmpty()) {
          // Preferred disambiguations
          for (String preferredDab : preferredDabs) {
            fixedBegin += addItem(
                submenu, null, preferredDab, true,
                new ReplaceTextAction(
                    page,
                    matcher.getReplacement(page, template, indexReplacement, preferredDab),
                    element, textPane));
          }
        } else {
          // Last replacement
          String title = LinkReplacement.getLastReplacement(disambigPage.getTitle());
          if (title != null) {
            fixedBegin += addItem(
                submenu, null, title, true,
                new ReplaceTextAction(
                    page,
                    matcher.getReplacement(page, template, indexReplacement, title),
                    element, textPane));
          }
        }

        // Wiktionary links
        if ((wiktionary != null) && (wiktionary.size() > 0)) {
          if ((!separators) && (fixedBegin > 0)) {
            fixedBegin += addSeparator(submenu);
            separators = true;
          }
          for (String wikt : wiktionary) {
            String name = "wikt:" + wikt;
            addItem(
                submenu, null, name, true,
                new ReplaceTextAction(
                    page,
                    matcher.getReplacement(page, template, indexReplacement, name),
                    element, textPane));
          }
        }

        // Possible links
        if ((links != null) && (links.size() > 0)) {
          if ((!separators) && (fixedBegin > 0)) {
            fixedBegin += addSeparator(submenu);
            separators = true;
          }
          for (Page p : links) {
            if (p.isRedirect()) {
              JMenu submenu1 = new JMenu(p.getTitle());
              Map<Page, List<String>> anchorsRedirectMap = new HashMap<Page, List<String>>();
              p.getLinksWithRedirect(anchorsRedirectMap);
              
              Iterator<Page> iter = p.getRedirectIteratorWithPage();
              while (iter.hasNext()) {
                Page pageTmp = iter.next();
                List<String> anchors = anchorsRedirectMap.get(pageTmp);

                addItem(
                    submenu1, pageTmp, null, true,
                    new ReplaceTextAction(
                        page,
                        matcher.getReplacement(page, template, indexReplacement, pageTmp.getTitle()),
                        element, textPane));
        
                if ((anchors != null) && (anchors.size() > 0)) {
                  for (String anchor : anchors) {
                    addItem(
                        submenu1, pageTmp, anchor, true,
                        new ReplaceTextAction(
                            page,
                            matcher.getReplacement(page, template, indexReplacement, anchor),
                            element, textPane));
                  }
                }
              }
              
              submenu.add(submenu1);
            } else {
              addItem(
                  submenu, p, null, true,
                  new ReplaceTextAction(
                      page,
                      matcher.getReplacement(page, template, indexReplacement, p.getTitle()),
                      element, textPane));

              // Anchors
              List<String> anchors = anchorsMap.get(p);
              if (anchors != null) {
                for (String anchor : anchors) {
                  addItem(
                      submenu, p, anchor, true,
                      new ReplaceTextAction(
                          page,
                          matcher.getReplacement(page, template, indexReplacement, anchor),
                          element, textPane));
                }
              }
            }
          }
        }

        if (separators) {
          if (!preferredDabs.isEmpty()) {
            // Preferred disambiguations
            fixedEnd += addSeparator(submenu);
            for (String preferredDab : preferredDabs) {
              fixedEnd += addItem(
                  submenu, null, preferredDab, true,
                  new ReplaceTextAction(
                      page,
                      matcher.getReplacement(page, template, indexReplacement, preferredDab),
                      element, textPane));
            }
          } else {
            // Last replacement
            String title = LinkReplacement.getLastReplacement(disambigPage.getTitle());
            if (title != null) {
              fixedEnd += addSeparator(submenu);
              fixedEnd += addItem(
                  submenu, null, title, true,
                  new ReplaceTextAction(
                      page,
                      matcher.getReplacement(page, template, indexReplacement, title),
                      element, textPane));
            }
          }
        }

        addSubmenu(popup, submenu, fixedBegin, fixedEnd);
      }
    }
  }

  /**
   * Create a text in the form {{template|pageTitle}} or {{template|pageTitle|text}}
   * 
   * @param template Template name.
   * @param pageTitle Page title.
   * @param text Text
   * @return Modified text.
   */
  private String createTextForTemplate(String template, String pageTitle, String text) {
    StringBuilder newText = new StringBuilder();
    newText.append("{{");
    newText.append(template);
    newText.append("|");
    newText.append(pageTitle);
    if ((text != null) && (!text.equals(pageTitle))) {
      newText.append("|");
      newText.append(text);
    }
    newText.append("}}");
    return newText.toString();
  }

  /**
   * Create a text in the form [[pageTitle]]{{template}} or [[pageTitle|text]]{{template}}
   * 
   * @param template Template name and optional arguments.
   * @param pageTitle Page title.
   * @param text Text
   * @return Modified text.
   */
  private String createTextForTemplateAfterLink(List<String> template, String pageTitle, String text) {
    StringBuilder newText = new StringBuilder();
    newText.append(PageElementInternalLink.createInternalLink(pageTitle, text));
    newText.append("{{");
    newText.append(template.get(0));
    for (int i = 1; i < template.size(); i++) {
      newText.append("|");
      newText.append(template.get(i));
    }
    newText.append("}}");
    return newText.toString();
  }
}
