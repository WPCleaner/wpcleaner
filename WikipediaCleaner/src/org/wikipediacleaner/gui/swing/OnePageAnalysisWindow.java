/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.Contributions;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.InternalLinkCount;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.User;
import org.wikipediacleaner.gui.swing.action.ActionDisambiguationAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.gui.swing.action.ActionFullAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionOccurrence;
import org.wikipediacleaner.gui.swing.action.ActionWatchPage;
import org.wikipediacleaner.gui.swing.action.SetComparatorAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.AbstractPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.AnalysisPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.CheckErrorPageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.CheckErrorPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.MWPaneCheckWikiFormatter;
import org.wikipediacleaner.gui.swing.component.MWPaneCheckWikiPopupListener;
import org.wikipediacleaner.gui.swing.component.MWPaneDisambiguationFormatter;
import org.wikipediacleaner.gui.swing.component.MWPaneDisambiguationPopupListener;
import org.wikipediacleaner.gui.swing.component.MWPanePopupListener;
import org.wikipediacleaner.gui.swing.component.PageListAnalyzeListener;
import org.wikipediacleaner.gui.swing.component.PageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;
import org.wikipediacleaner.gui.swing.menu.BasicMenuCreator;
import org.wikipediacleaner.gui.swing.worker.FullAnalysisWorker;
import org.wikipediacleaner.gui.swing.worker.TranslateWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Analysis window.
 */
public class OnePageAnalysisWindow extends OnePageWindow {

  private JButton buttonFirst;
  private JButton buttonPrevious;
  private JButton buttonNext;
  private JButton buttonLast;
  private JButton buttonToc;
  private JButton buttonDelete;
  private JButton buttonValidate;
  private JButton buttonView;
  private JButton buttonViewHistory;
  private JButton buttonWatch;

  JList listLinks;
  PageListCellRenderer listCellRenderer;
  PageListModel modelLinks;
  Map<String, Integer> mapLinksTotalCount;
  Map<String, Integer> mapLinksHelpNeededCount;
  private AbstractPageListPopupListener popupListenerLinks;
  JCheckBoxMenuItem menuItemShowDisambiguation;
  JCheckBoxMenuItem menuItemShowMissing;
  JCheckBoxMenuItem menuItemShowOther;
  JCheckBoxMenuItem menuItemShowRedirect;
  JCheckBoxMenuItem menuItemCountDisambiguation;
  JCheckBoxMenuItem menuItemCountMissing;
  JCheckBoxMenuItem menuItemCountOther;
  JCheckBoxMenuItem menuItemCountRedirect;
  private JButton buttonRemoveLinks;
  private JButton buttonWatchLink;
  private JButton buttonDisambiguationWarning;
  private JButton buttonRedirectElements;
  private JButton buttonOtherLanguage;
  private JButton buttonTranslation;

  List<CheckErrorAlgorithm> allAlgorithms;
  JList listErrors;
  private DefaultListModel modelErrors;

  List<Page> knownPages;

  boolean translated;

  private List<String> addedCategories;
  private List<String> addedTemplates;

  /**
   * Create and display a AnalysisWindow.
   * 
   * @param page Page name.
   * @param knownPages Pages already loaded.
   * @param wikipedia Wikipedia.
   */
  public static void createAnalysisWindow(
      final String page,
      final List<Page> knownPages,
      final EnumWikipedia wikipedia) {
    createWindow(
        "AnalysisWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        OnePageAnalysisWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof OnePageAnalysisWindow) {
              Configuration config = Configuration.getConfiguration();
              OnePageAnalysisWindow analysis = (OnePageAnalysisWindow) window;
              analysis.setPageName(page);
              analysis.knownPages = knownPages;
              analysis.modelLinks = new PageListModel();
              analysis.modelLinks.setShowDisambiguation(config.getBoolean(
                  null,
                  ConfigurationValueBoolean.ANALYSIS_DISAMBIG_PAGES));
              analysis.modelLinks.setShowMissing(config.getBoolean(
                  null,
                  ConfigurationValueBoolean.ANALYSIS_MISSING_PAGES));
              analysis.modelLinks.setShowOther(config.getBoolean(
                  null,
                  ConfigurationValueBoolean.ANALYSIS_OTHER_PAGES));
              analysis.modelLinks.setShowRedirect(config.getBoolean(
                  null,
                  ConfigurationValueBoolean.ANALYSIS_REDIRECT_PAGES));
              analysis.modelLinks.setCountDisambiguation(config.getBoolean(
                  null,
                  ConfigurationValueBoolean.ANALYSIS_COUNT_DISAMBIG));
              analysis.modelLinks.setCountMissing(config.getBoolean(
                  null,
                  ConfigurationValueBoolean.ANALYSIS_COUNT_MISSING));
              analysis.modelLinks.setCountOther(config.getBoolean(
                  null,
                  ConfigurationValueBoolean.ANALYSIS_COUNT_OTHER));
              analysis.modelLinks.setCountRedirect(config.getBoolean(
                  null,
                  ConfigurationValueBoolean.ANALYSIS_COUNT_REDIRECT));
              analysis.modelLinks.setComparator(PageComparator.getNamespaceFirstComparator());
              analysis.createTextContents(window);
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof OnePageAnalysisWindow) {
              OnePageAnalysisWindow analysis = (OnePageAnalysisWindow) window;
              analysis.actionReload();
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Analysis - {0}", getPageName());
  }

  /**
   * @return Menu bar.
   */
  @Override
  protected JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    menuBar.add(createToolsMenu());
    menuBar.add(createOptionsMenu());
    menuBar.add(createSortMenu());
    menuBar.add(Box.createHorizontalGlue());
    JLabel linkCount = new JLabel(GT._("Link count"));
    modelLinks.setLinkCountLabel(linkCount);
    menuBar.add(linkCount);
    return menuBar;
  }

  /**
   * Update component state.
   */
  @Override
  protected void updateComponentState() {
    boolean article = (isPageLoaded()) && (getPage() != null) && (getPage().isArticle());
    boolean redirect = article && getPage().isRedirect();
    setEnabledStatus(buttonDelete, isPageLoaded());
    setEnabledStatus(buttonDisambiguationWarning, article);
    setEnabledStatus(buttonFirst, isPageLoaded());
    setEnabledStatus(buttonLast, isPageLoaded());
    setEnabledStatus(buttonNext, isPageLoaded());
    setEnabledStatus(buttonOtherLanguage, isPageLoaded());
    setEnabledStatus(buttonPrevious, isPageLoaded());
    setEnabledStatus(buttonRedirectElements, redirect);
    setVisibleStatus(buttonRedirectElements, redirect);
    setEnabledStatus(buttonToc, isPageLoaded());
    setEnabledStatus(buttonTranslation, isPageLoaded());
    setEnabledStatus(buttonValidate, isPageLoaded());
    setEnabledStatus(buttonView, isPageLoaded());
    setEnabledStatus(buttonViewHistory, isPageLoaded());
    setEnabledStatus(buttonWatch, isPageLoaded());
    super.updateComponentState();
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(2, 2, 2, 2);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Page name
    //constraints.gridwidth = 2;
    panel.add(createPageComponents(), constraints);
    constraints.gridy++;

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    JSplitPane splitLinks = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
    splitLinks.setLeftComponent(createLinksComponents());
    splitLinks.setRightComponent(createCheckWikiComponents());
    splitLinks.setResizeWeight(1.0);
    splitLinks.setDividerLocation(0.9);
    JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    split.setLeftComponent(splitLinks);
    split.setRightComponent(createContentsComponents());
    split.setPreferredSize(new Dimension(1200, 700));
    split.setMinimumSize(new Dimension(200, 200));
    split.setResizeWeight(0.0);
    split.setDividerLocation(200 + split.getInsets().left);
    panel.add(split, constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * @return Page components.
   */
  private Component createPageComponents() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    JPanel panelInformation = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
    JPanel panelComment = new JPanel(new GridBagLayout());

    // Page name
    addTextPageName(panelInformation);

    // Check box for closing after sending
    addChkCloseAfterSend(panelInformation);

    // Check box for adding a note on the talk page
    addChkEditTalkPage(panelInformation);

    // Check box for updating and creating disambiguation warning on the talk page
    addChkUpdateDabWarning(panelInformation);
    addChkCreateDabWarning(panelInformation);

    // Comment
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    addChkAutomaticComment(panelComment, constraints);
    setComment("");

    panel.add(panelInformation);
    panel.add(panelComment);

    return panel;
  }

  /**
   * @return Contents components.
   */
  private Component createContentsComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Text buttons
    JToolBar toolbarButtons = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarButtons.setFloatable(false);
    buttonFirst = ActionOccurrence.addButton(
        toolbarButtons, getTextContents(), ActionOccurrence.Occurrence.FIRST, true, true);
    buttonPrevious = ActionOccurrence.addButton(
        toolbarButtons, getTextContents(), ActionOccurrence.Occurrence.PREVIOUS, true, true);
    buttonNext = ActionOccurrence.addButton(
        toolbarButtons, getTextContents(), ActionOccurrence.Occurrence.NEXT, true, true);
    buttonLast = ActionOccurrence.addButton(
        toolbarButtons, getTextContents(), ActionOccurrence.Occurrence.LAST, true, true);
    toolbarButtons.addSeparator();
    addButtonUndoRedo(toolbarButtons, true);
    buttonToc = createButtonToc(this, true);
    toolbarButtons.add(buttonToc);
    addChkSpelling(toolbarButtons, true);
    buttonRedirectElements = Utilities.createJButton(
        "commons-nuvola-apps-kpager.png", EnumImageSize.NORMAL,
        GT._("Add categories or templates"), false, null);
    buttonRedirectElements.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRedirectElements"));
    toolbarButtons.add(buttonRedirectElements);
    buttonValidate = createButtonValidate(this, true);
    toolbarButtons.add(buttonValidate);
    addButtonSend(toolbarButtons, true);
    if ((getWikipedia().getConnection().getUser() != null) &&
        (getWikipedia().getConnection().getUser().hasRight(User.RIGHT_DELETE))) {
      buttonDelete = Utilities.createJButton(
          "gnome-edit-delete.png", EnumImageSize.NORMAL,
          GT._("Delete page"), false, null);
      buttonDelete.addActionListener(EventHandler.create(
          ActionListener.class, this, "actionDelete"));
      toolbarButtons.add(buttonDelete);
    }
    addButtonRedirect(toolbarButtons);
    buttonDisambiguationWarning = Utilities.createJButton(
        "gnome-dialog-warning.png", EnumImageSize.NORMAL,
        GT._("Add a warning on the talk page about the links to disambiguation pages"),
        false, null); 
    buttonDisambiguationWarning.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDisambiguationWarning"));
    toolbarButtons.add(buttonDisambiguationWarning);
    toolbarButtons.addSeparator();
    addButtonReload(toolbarButtons, true);
    buttonView = ActionExternalViewer.addButton(
        toolbarButtons, getWikipedia(), getPageName(), false, true, false);
    buttonViewHistory = ActionExternalViewer.addButton(
        toolbarButtons, getWikipedia(), getPageName(),
        ActionExternalViewer.ACTION_HISTORY, true, false);
    toolbarButtons.addSeparator();
    buttonWatch = ActionWatchPage.addButton(
        getParentComponent(), toolbarButtons, getWikipedia(), getPageName(), true, false);
    addButtonDisambiguation(toolbarButtons, true);
    toolbarButtons.addSeparator();
    String[] elements = getConfiguration().getStringArray(WPCConfigurationString.LANG_TEMPLATE);
    String langTemplateName = "lang";
    if ((elements != null) && (elements.length > 0)) {
      langTemplateName = elements[0];
    }
    buttonOtherLanguage = Utilities.createJButton("<html><b>{{" + langTemplateName + "}}</b></html>", null);
    buttonOtherLanguage.setToolTipText(GT._("Mark the selected text as being in a foreign language"));
    buttonOtherLanguage.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionOtherLanguage"));
    toolbarButtons.add(buttonOtherLanguage);
    buttonTranslation = Utilities.createJButton(
        "<html><b>(??)</b> \u21d2 <b>(" + getWikipedia().getSettings().getLanguage() + ")</b></html>", null);
    buttonTranslation.setToolTipText(GT._("Translation of an article copied from an other wiki"));
    buttonTranslation.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionTranslate"));
    toolbarButtons.add(buttonTranslation);
    toolbarButtons.addSeparator();
    addLblLastModified(toolbarButtons);
    toolbarButtons.addSeparator();
    addLblEditProtectionLevel(toolbarButtons);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(toolbarButtons, constraints);
    constraints.gridy++;

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    addTextContents(panel, constraints);
    constraints.gridy++;
    listErrors.addMouseListener(
        new CheckErrorPageListPopupListener(getWikipedia(), getTextContents(), buttonValidate));

    return panel;
  }

  /**
   * @return Sort menu.
   */
  private JMenu createSortMenu() {
    JMenu menu = Utilities.createJMenu(GT._("Sort"));
    List<CompositeComparator<Page>> comparators = PageComparator.getComparators();
    for (CompositeComparator<Page> comparator : comparators) {
      JMenuItem menuItem = Utilities.createJMenuItem(comparator.getName(), true);
      menuItem.addActionListener(new SetComparatorAction(modelLinks, comparator));
      menu.add(menuItem);
    }
    return menu;
  }

  /**
   * @return Options menu.
   */
  private JMenu createOptionsMenu() {
    JMenu menu = Utilities.createJMenu(GT._("&Options"));
    menuItemShowDisambiguation = Utilities.createJCheckBoxMenuItm(
        GT._("Show &disambiguation pages"), modelLinks.getShowDisambiguation());
    menuItemShowDisambiguation.addItemListener(this);
    menu.add(menuItemShowDisambiguation);
    menuItemShowMissing = Utilities.createJCheckBoxMenuItm(
        GT._("Show &missing pages"), modelLinks.getShowMissing());
    menuItemShowMissing.addItemListener(this);
    menu.add(menuItemShowMissing);
    menuItemShowRedirect = Utilities.createJCheckBoxMenuItm(
        GT._("Show &redirect pages"), modelLinks.getShowRedirect());
    menuItemShowRedirect.addItemListener(this);
    menu.add(menuItemShowRedirect);
    menuItemShowOther = Utilities.createJCheckBoxMenuItm(
        GT._("Show &other pages"), modelLinks.getShowOther());
    menuItemShowOther.addItemListener(this);
    menu.add(menuItemShowOther);
    menu.add(new JSeparator());
    menuItemCountDisambiguation = Utilities.createJCheckBoxMenuItm(
        GT._("Count disambiguation pages"), modelLinks.getCountDisambiguation());
    menuItemCountDisambiguation.addItemListener(this);
    menu.add(menuItemCountDisambiguation);
    menuItemCountMissing = Utilities.createJCheckBoxMenuItm(
        GT._("Count missing pages"), modelLinks.getCountMissing());
    menuItemCountMissing.addItemListener(this);
    menu.add(menuItemCountMissing);
    menuItemCountRedirect = Utilities.createJCheckBoxMenuItm(
        GT._("Count redirect pages"), modelLinks.getCountRedirect());
    menuItemCountRedirect.addItemListener(this);
    menu.add(menuItemCountRedirect);
    menuItemCountOther = Utilities.createJCheckBoxMenuItm(
        GT._("Count other pages"), modelLinks.getCountOther());
    menuItemCountOther.addItemListener(this);
    menu.add(menuItemCountOther);
    return menu;
  }

  /**
   * @return Links components.
   */
  private Component createLinksComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    listLinks = new JList(modelLinks);

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Button toolbar
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    ActionFullAnalysis.addButton(
        getParentComponent(), toolbar, getWikipedia(), listLinks, knownPages, true, true);
    ActionDisambiguationAnalysis.addButton(
        getParentComponent(), toolbar, getWikipedia(), listLinks, true, true);
    buttonRemoveLinks = Utilities.createJButton(
        "wpc-remove-link.png", EnumImageSize.NORMAL,
        GT._("Remove all links"), false, null);
    buttonRemoveLinks.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRemoveAllLinks"));
    toolbar.add(buttonRemoveLinks);
    buttonWatchLink = ActionWatchPage.createButton(
        getParentComponent(), getWikipedia(), listLinks, true, true);
    toolbar.add(buttonWatchLink);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    panel.add(toolbar, constraints);
    constraints.gridy++;

    // Links
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    listCellRenderer = new PageListCellRenderer();
    listCellRenderer.showCountOccurrence(true);
    listCellRenderer.showDisambiguation(true);
    listCellRenderer.showMissing(true);
    listCellRenderer.showRedirect(true);
    listLinks.setCellRenderer(listCellRenderer);
    popupListenerLinks = new AnalysisPageListPopupListener(
        getWikipedia(), getTextContents(), listLinks, this);
    listLinks.addMouseListener(popupListenerLinks);
    listLinks.addMouseListener(new PageListAnalyzeListener(getWikipedia(), null));
    listLinks.addListSelectionListener(new AnalysisListSelectionListener());
    JScrollPane scrollLinks = new JScrollPane(listLinks);
    scrollLinks.setMinimumSize(new Dimension(100, 100));
    scrollLinks.setPreferredSize(new Dimension(200, 500));
    scrollLinks.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollLinks, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Check Wiki components.
   */
  private Component createCheckWikiComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Initialize algorithms list
    allAlgorithms = CheckErrorAlgorithms.getAlgorithms(getWikipedia());
    if (allAlgorithms == null) {
      allAlgorithms = Collections.emptyList();
    }

    // Title
    JLabel labelErrors = Utilities.createJLabel(GT._("Check Wikipedia"));
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(labelErrors, constraints);
    constraints.gridy++;

    // Errors list
    modelErrors = new DefaultListModel();
    listErrors = new JList(modelErrors);
    CheckErrorPageListCellRenderer cellRenderer = new CheckErrorPageListCellRenderer(false);
    cellRenderer.showCountOccurence(true);
    listErrors.setCellRenderer(cellRenderer);
    listErrors.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    listErrors.addListSelectionListener(new AnalysisListSelectionListener());
    JScrollPane scrollErrors = new JScrollPane(listErrors);
    scrollErrors.setMinimumSize(new Dimension(200, 100));
    scrollErrors.setPreferredSize(new Dimension(200, 200));
    scrollErrors.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(scrollErrors, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @param list List to use for formatting.
   */
  void updateTextFormatting(JList list) {
    if (list == null) {
      Object[] selection = listLinks.getSelectedValues();
      if ((selection != null) && (selection.length > 0)) {
        list = listLinks;
      }
    }
    if (list == null) {
      Object[] selection = listErrors.getSelectedValues();
      if ((selection != null) && (selection.length > 0)) {
        list = listErrors;
      }
    }
    if (list == listLinks) {
      // List of links
      Object[] selection = listLinks.getSelectedValues();
      if ((selection != null) && (selection.length > 0)) {
        listErrors.clearSelection();
        List<Page> pages = new ArrayList<Page>();
        for (int i = 0; i < selection.length; i++) {
          if (selection[i] instanceof Page) {
            pages.add((Page) selection[i]);
          }
        }
        MWPaneFormatter formatter = getTextContents().getFormatter();
        if (formatter instanceof MWPaneDisambiguationFormatter) {
          MWPaneDisambiguationFormatter dabFormatter =
            (MWPaneDisambiguationFormatter) formatter;
          if (!dabFormatter.isSameList(pages)) {
            formatter = new MWPaneDisambiguationFormatter(getWikipedia(), pages);
            getTextContents().setFormatter(formatter);
          }
        } else {
          formatter = new MWPaneDisambiguationFormatter(getWikipedia(), pages);
          getTextContents().setFormatter(formatter);
        }
        MWPanePopupListener listener = new MWPaneDisambiguationPopupListener(
            getWikipedia(), OnePageAnalysisWindow.this);
        getTextContents().setPopupListener(listener);
      }
    } else if (list == listErrors) {
      // List of errors
      Object selection = listErrors.getSelectedValue();
      if ((selection != null) && (selection instanceof CheckErrorPage)) {
        listLinks.clearSelection();
        CheckErrorPage errorSelected = (CheckErrorPage) selection;
        MWPaneFormatter formatter = getTextContents().getFormatter();
        if (formatter instanceof MWPaneCheckWikiFormatter) {
          MWPaneCheckWikiFormatter cwFormatter =
            (MWPaneCheckWikiFormatter) formatter;
          if (!cwFormatter.isSameAlgorithm(errorSelected.getAlgorithm())) {
            formatter = new MWPaneCheckWikiFormatter(errorSelected.getAlgorithm());
            getTextContents().setFormatter(formatter);
          }
        } else {
          formatter = new MWPaneCheckWikiFormatter(errorSelected.getAlgorithm());
          getTextContents().setFormatter(formatter);
        }
        MWPanePopupListener listener = new MWPaneCheckWikiPopupListener(
            getWikipedia(), OnePageAnalysisWindow.this);
        getTextContents().setPopupListener(listener);
      }
    }
    if (list != null) {
      list.repaint();
    }
    updateComponentState();
  }

  /**
   * A ListSelectionListener implementation. 
   */
  class AnalysisListSelectionListener implements ListSelectionListener {

    public void valueChanged(ListSelectionEvent e) {
      if (e.getSource() instanceof JList) {
        JList list = (JList) e.getSource();
        updateTextFormatting(list);
      }
    }
  }

  /* (non-Javadoc)
   * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
   */
  @Override
  public void itemStateChanged(ItemEvent e) {
    if ((e == null) || (e.getSource() == null)) {
      return;
    }
    super.itemStateChanged(e);
    Object source = e.getSource();
    boolean selected = (e.getStateChange() == ItemEvent.SELECTED);
    if (source == menuItemShowDisambiguation) {
      modelLinks.setShowDisambiguation(selected);
    } else if (source == menuItemShowMissing) {
      modelLinks.setShowMissing(selected);
    } else if (source == menuItemShowOther) {
      modelLinks.setShowOther(selected);
    } else if (source == menuItemShowRedirect) {
      modelLinks.setShowRedirect(selected);
    } else if (source == menuItemCountDisambiguation) {
      modelLinks.setCountDisambiguation(selected);
    } else if (source == menuItemCountMissing) {
      modelLinks.setCountMissing(selected);
    } else if (source == menuItemCountOther) {
      modelLinks.setCountOther(selected);
    } else if (source == menuItemCountRedirect) {
      modelLinks.setCountRedirect(selected);
    }
  }

  /**
   * Clean page. 
   */
  @Override
  protected void clean() {
    super.clean();
    popupListenerLinks.setPage(getPage());
    modelLinks.clear();
    modelErrors.clear();
    updateComponentState();
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();
    FullAnalysisWorker reloadWorker = new FullAnalysisWorker(
        getWikipedia(), this, getPage(), knownPages, allAlgorithms);
    knownPages = null;
    setupReloadWorker(reloadWorker);
    reloadWorker.start();
  }

  private boolean firstReload = true;

  /**
   * Callback called at the end of the Reload Worker.
   */
  @Override
  protected void afterFinishedReloadWorker() {
    super.afterFinishedReloadWorker();

    // Clean up
    listLinks.clearSelection();
    listErrors.clearSelection();

    // Update links information
    Page page = getPage();
    PageAnalysis analysis = page.getAnalysis(page.getContents(), true);
    mapLinksTotalCount = new HashMap<String, Integer>();
    mapLinksHelpNeededCount = new HashMap<String, Integer>();
    if (page.getLinks() != null) {
      List<Page> links = page.getLinks();
      modelLinks.setElements(links);
      countOccurrences(analysis, true);
      for (Page p : links) {
        if ((p != null) &&
            (Boolean.TRUE.equals(p.isDisambiguationPage()))) {
          InternalLinkCount count = analysis.getLinkCount(p);
          if ((count != null) && (count.getTotalLinkCount() > 0)) {
            mapLinksTotalCount.put(p.getTitle(), Integer.valueOf(count.getTotalLinkCount()));
            mapLinksHelpNeededCount.put(p.getTitle(), Integer.valueOf(count.getHelpNeededCount()));
          }
        }
      }
    }
    selectLinks(0);
    modelLinks.updateLinkCount();

    // Update fix redirects menu
    createFixRedirectsMenu();

    // Check Wiki
    modelErrors.clear();
    initializeInitialErrors(allAlgorithms);
    if (getInitialErrors() != null) {
      for (CheckErrorPage error : getInitialErrors()) {
        modelErrors.addElement(error);
      }
    }
    listErrors.clearSelection();

    if (firstReload) {
      firstReload = false;

      // Check page contents
      Page pageLoaded = getPage();
      if ((pageLoaded != null) &&
          (Boolean.FALSE.equals(pageLoaded.isExisting()))) {
        List<Page> similarPages = pageLoaded.getRelatedPages(Page.RelatedPages.SIMILAR_PAGES);
        if ((similarPages != null) && (similarPages.size() > 0)) {
          String answer = Utilities.askForValue(
              getParentComponent(),
              GT._(
                  "Page {0} doesn''t exist, do you want to load a similar page ?",
                  pageLoaded.getTitle()),
              similarPages.toArray(), true,
              similarPages.get(0).toString(), null);
          if ((answer != null) && (answer.trim().length() > 0)) {
            Controller.runFullAnalysis(answer, null, getWikipedia());
          }
          dispose();
          return;
        }
        int answer = Utilities.displayYesNoWarning(
            getParentComponent(),
            GT._(
                "Page {0} doesn''t exist, do you still want to analyze it ?",
                pageLoaded.getTitle()));
        if (answer != JOptionPane.YES_OPTION) {
          dispose();
          return;
        }
      }

      // Configuration depending on the type of page
      boolean isArticle = (getPage() != null) && (getPage().isArticle());
      if (isArticle) {
        Configuration config = Configuration.getConfiguration();
        if (getPage().isInMainNamespace()) {
          chkUpdateDabWarning.setSelected(config.getBoolean(
              null,
              ConfigurationValueBoolean.UPDATE_DAB_WARNING));
          chkCreateDabWarning.setSelected(config.getBoolean(
              null,
              ConfigurationValueBoolean.CREATE_DAB_WARNING));
        } else if ((getPage().getNamespace() != null) &&
                   (getWikipedia().getConfiguration().isEncyclopedicNamespace(
                       getPage().getNamespace()))) {
          chkUpdateDabWarning.setSelected(config.getBoolean(
              null,
              ConfigurationValueBoolean.UPDATE_DAB_WARNING_ENCY));
          chkCreateDabWarning.setSelected(config.getBoolean(
              null,
              ConfigurationValueBoolean.CREATE_DAB_WARNING_ENCY));
        } else {
          chkUpdateDabWarning.setSelected(config.getBoolean(
              null,
              ConfigurationValueBoolean.UPDATE_DAB_WARNING_ALL));
          chkCreateDabWarning.setSelected(config.getBoolean(
              null,
              ConfigurationValueBoolean.CREATE_DAB_WARNING_ALL));
        }
      }
    }
    updateComponentState();

    // Edit warning if needed
    boolean automaticFix = true;
    WPCConfiguration wpcConfig = getWikipedia().getConfiguration();
    List<String[]> warningTemplates = wpcConfig.getStringArrayList(
        WPCConfigurationStringList.EDIT_WARNING_TEMPLATES);
    if ((warningTemplates != null) && (!warningTemplates.isEmpty())) {
      StringBuilder tmp = new StringBuilder(GT._(
          "\"{0}\" has been tagged with the following templates, be careful when editing:",
          getPage().getTitle()));
      boolean found = false;
      for (String[] warningTemplate : warningTemplates) {
        if ((warningTemplate.length > 0) && (warningTemplate[0] != null)) {
          List<PageElementTemplate> templates = analysis.getTemplates(warningTemplate[0]);
          for (PageElementTemplate template : templates) {
            tmp.append("\n* ");
            tmp.append(template.getTemplateName());
            if ((warningTemplate.length > 1) && (warningTemplate[1] != null)) {
              String message = template.getParameterValue(warningTemplate[1]);
              if (message != null) {
                tmp.append(", ");
                tmp.append(message);
              }
              found = true;
            }
          }
        }
      }
      if (found) {
        tmp.append("\n");
        tmp.append(GT._("Do you want to apply the automatic modifications?"));
        int answer = displayYesNoWarning(tmp.toString());
        if (answer != JOptionPane.YES_OPTION) {
          automaticFix = false;
        }
      }
    }

    // Automatic fix of some errors
    if (automaticFix &&
        (getInitialErrors() != null) &&
        (getTextContents() != null)) {
      String initialContents = getTextContents().getText();
      String contents = initialContents;
      for (CheckErrorPage error : getInitialErrors()) {
        analysis = getPage().getAnalysis(contents, true);
        contents = error.getAlgorithm().automaticFix(analysis);
      }
      if (!contents.equals(initialContents)) {
        getTextContents().changeText(contents);
        actionValidate(false);
      }
    }
  }

  /**
   * Callback called before the end of the Reload Worker.
   */
  @Override
  protected void beforeFinishedReloadWorker() {
    super.beforeFinishedReloadWorker();
    modelLinks.setShowDisambiguation(menuItemShowDisambiguation.isSelected());
    modelLinks.setShowMissing(menuItemShowMissing.isSelected());
    modelLinks.setShowOther(menuItemShowOther.isSelected());
    modelLinks.setShowRedirect(menuItemShowRedirect.isSelected());
  }

  /**
   * Callback called before the start of the Reload Worker. 
   */
  @Override
  protected void beforeStartReloadWorker() {
    super.beforeStartReloadWorker();
    modelLinks.setShowDisambiguation(true);
    modelLinks.setShowMissing(true);
    modelLinks.setShowOther(true);
    modelLinks.setShowRedirect(true);
  }

  /**
   * Select indexes.
   * 
   * @param startIndex First index to select.
   */
  void selectLinks(int startIndex) {
    if (modelLinks.getSize() == 0) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    int maxSelections = config.getInt(
        null,
        ConfigurationValueInteger.ANALYSIS_NB_PAGES);
    int nbSelections = Math.max(Math.min(maxSelections, modelLinks.getSize() - startIndex), 1);
    startIndex = Math.min(startIndex, modelLinks.getSize() - nbSelections);
    listLinks.getSelectionModel().setSelectionInterval(startIndex, startIndex + nbSelections - 1);
    listLinks.ensureIndexIsVisible(startIndex);
    listLinks.ensureIndexIsVisible(startIndex + nbSelections - 1);
  }

  /**
   * Action called when Remove all links button is pressed.
   */
  public void actionRemoveAllLinks() {
    Object[] selected = listLinks.getSelectedValues();
    if ((selected == null) || (selected.length == 0)) {
      return;
    }
    List<String> titles = new ArrayList<String>();
    for (Object selectedLine : selected) {
      if (selectedLine instanceof Page) {
        titles.add(((Page) selectedLine).getTitle());
      }
    }
    if (titles.size() == 0) {
      return;
    }
    String currentText = getTextContents().getText();
    PageAnalysis analysis = getPage().getAnalysis(currentText, false);
    StringBuilder buffer = new StringBuilder();
    int lastPosition = 0;
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    for (PageElementInternalLink link : links) {
      boolean shouldChange = false;
      for (String title : titles) {
        if (Page.areSameTitle(title, link.getLink())) {
          shouldChange = true;
        }
      }
      if (shouldChange) {
        buffer.append(currentText.substring(lastPosition, link.getBeginIndex()));
        lastPosition = link.getBeginIndex();
        buffer.append(link.getDisplayedText());
        lastPosition = link.getEndIndex();
      }
    }
    if (lastPosition > 0) {
      buffer.append(currentText.substring(lastPosition));
      getTextContents().changeText(buffer.toString());
    }
    actionValidate(true);
  }

  /**
   * Action called when Disambiguation warning button is pressed.  
   */
  public void actionDisambiguationWarning() {
    // Check configuration
    String template = getConfiguration().getString(WPCConfigurationString.DAB_WARNING_TEMPLATE);
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayMessageForMissingConfiguration(
          getParentComponent(),
          WPCConfigurationString.DAB_WARNING_TEMPLATE.getAttributeName());
      return;
    }

    int answer = Utilities.displayYesNoWarning(
        getParentComponent(),
        GT._("Do you want to update the disambiguation warning in talk page ?"));
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }
    UpdateDabWarningWorker worker = new UpdateDabWarningWorker(
        getWikipedia(), this,
        Collections.singletonList(getPage()),
        true, true, true, false);
    worker.start();
  }

  /**
   * Action called when when Add redirect categories or templates button is pressed.
   */
  public void actionRedirectElements() {

    // Check configuration
    List<String> redirectCategories = getConfiguration().getStringList(
        WPCConfigurationStringList.REDIRECT_CATEGORIES);
    List<String> redirectTemplates = getConfiguration().getStringList(
        WPCConfigurationStringList.REDIRECT_TEMPLATES);
    if (((redirectCategories == null) || (redirectCategories.isEmpty())) &&
        ((redirectTemplates == null) || (redirectTemplates.isEmpty()))) {
      List<String> params = new ArrayList<String>();
      params.add(WPCConfigurationStringList.REDIRECT_CATEGORIES.getAttributeName());
      params.add(WPCConfigurationStringList.REDIRECT_TEMPLATES.getAttributeName());
      Utilities.displayMessageForMissingConfiguration(
          getParentComponent(), params);
      return;
    }

    // Group by theme if available
    Map<String, JMenu> themeMenus = new HashMap<String, JMenu>();

    // Create menu
    List<JMenuItem> items = new ArrayList<JMenuItem>();
    if (redirectCategories != null) {
      for (String category : redirectCategories) {
        int colonIndex = category.indexOf(':');
        JMenu themeMenu = null;
        if (colonIndex > 0) {
          String theme = category.substring(0, colonIndex);
          category = category.substring(colonIndex + 1);
          themeMenu = themeMenus.get(theme);
          if (themeMenu == null) {
            themeMenu = new JMenu(theme);
            themeMenus.put(theme, themeMenu);
            items.add(themeMenu);
          }
        }
        JMenuItem item = new JMenuItem(category);
        item.setActionCommand(category);
        item.addActionListener(EventHandler.create(
            ActionListener.class, this, "actionAddCategory", "actionCommand"));
        if (themeMenu == null) {
          items.add(item);
        } else {
          themeMenu.add(item);
        }
      }
    }
    if (redirectTemplates != null) {
      for (String template : redirectTemplates) {
        int colonIndex = template.indexOf(':');
        JMenu themeMenu = null;
        if (colonIndex > 0) {
          String theme = template.substring(0, colonIndex);
          template = template.substring(colonIndex + 1);
          themeMenu = themeMenus.get(theme);
          if (themeMenu == null) {
            themeMenu = new JMenu(theme);
            themeMenus.put(theme, themeMenu);
            items.add(themeMenu);
          }
        }
        JMenuItem item = new JMenuItem("{{" + template + "}}");
        item.setActionCommand(template);
        item.addActionListener(EventHandler.create(
            ActionListener.class, this, "actionAddTemplate", "actionCommand"));
        if (themeMenu == null) {
          items.add(item);
        } else {
          themeMenu.add(item);
        }
      }
    }
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(null);
    menu.addSubmenus(popup, items);
    popup.show(buttonRedirectElements, 0, buttonRedirectElements.getHeight());
  }

  /**
   * Action called when a category is selected to be added.
   * 
   * @param categoryName Category name.
   */
  public void actionAddCategory(String categoryName) {
    if ((categoryName == null) || (getPage() == null)) {
      return;
    }
    String contents = getTextContents().getText();
    PageAnalysis analysis = getPage().getAnalysis(contents, false);

    // Check that the category isn't already applied
    List<PageElementCategory> categories = analysis.getCategories();
    for (PageElementCategory category : categories) {
      if (Page.areSameTitle(categoryName, category.getCategory())) {
        return;
      }
    }

    // Find where to add the category
    int index = contents.length();
    if (!categories.isEmpty()) {
      index = categories.get(categories.size() - 1).getEndIndex();
    } else {
      List<PageElementLanguageLink> langLinks = analysis.getLanguageLinks();
      if ((langLinks != null) && (!langLinks.isEmpty())) {
        index = langLinks.get(0).getBeginIndex();
      }
    }

    // Add the category
    StringBuilder newContents = new StringBuilder();
    if (index > 0) {
      newContents.append(contents.substring(0, index));
    }
    newContents.append("\n");
    newContents.append(PageElementCategory.createCategory(getWikipedia(), categoryName, null));
    if (index < contents.length()) {
      if (contents.charAt(index) != '\n') {
        newContents.append('\n');
      }
      newContents.append(contents.substring(index));
    }
    getTextContents().changeText(newContents.toString());
    if (addedCategories == null) {
      addedCategories = new ArrayList<String>();
    }
    addedCategories.add(categoryName);
    actionValidate(true);
  }

  /**
   * Action called when a template is selected to be added.
   * 
   * @param templateName Template name.
   */
  public void actionAddTemplate(String templateName) {
    if ((templateName == null) || (getPage() == null)) {
      return;
    }
    String contents = getTextContents().getText();
    PageAnalysis analysis = getPage().getAnalysis(contents, false);

    // Check that the template isn't already applied
    List<PageElementTemplate> templates = analysis.getTemplates(templateName);
    if ((templates != null) && (!templates.isEmpty())) {
      return;
    }

    // Find where to add the template
    int crBefore = 0;
    int crAfter = 2;
    int index = contents.length();
    templates = analysis.getTemplates();
    if ((templates != null) && (!templates.isEmpty())) {
      index = templates.get(0).getBeginIndex();
      crAfter = 1;
      int indexNewLine = contents.indexOf('\n');
      if ((indexNewLine > 0) && (indexNewLine > index)) {
        crBefore = 2;
      }
    } else {
      List<PageElementCategory> categories = analysis.getCategories();
      if ((categories != null) && (!categories.isEmpty())) {
        index = categories.get(0).getBeginIndex();
      } else {
        List<PageElementLanguageLink> langLinks = analysis.getLanguageLinks();
        if ((langLinks != null) && (!langLinks.isEmpty())) {
          index = langLinks.get(0).getBeginIndex();
        } else {
          int indexNewLine = contents.indexOf('\n');
          if (indexNewLine > 0) {
            index = indexNewLine;
          }
          crBefore = 2;
          crAfter = 0;
        }
      }
    }

    // Add the template
    StringBuilder newContents = new StringBuilder();
    if (index > 0) {
      newContents.append(contents.substring(0, index));
    }
    for (int i = 0; i < crBefore; i++) {
      newContents.append("\n");
    }
    newContents.append("{{");
    newContents.append(templateName);
    newContents.append("}}");
    for (int i = 0; i < crAfter; i++) {
      newContents.append("\n");
    }
    if (index < contents.length()) {
      newContents.append(contents.substring(index));
    }
    getTextContents().changeText(newContents.toString());
    if (addedTemplates == null) {
      addedTemplates = new ArrayList<String>();
    }
    addedTemplates.add(templateName);
    actionValidate(true);
  }

  /**
   * Action called when Other language button is pressed.
   */
  public void actionOtherLanguage() {
    // Check configuration
    String[] elements = getConfiguration().getStringArray(WPCConfigurationString.LANG_TEMPLATE);
    if ((elements == null) || (elements.length == 0)) {
      Utilities.displayMessageForMissingConfiguration(
          getParentComponent(),
          WPCConfigurationString.LANG_TEMPLATE.getAttributeName());
      return;
    }

    // Check selection
    String text = getTextContents().getText();
    int start = getTextContents().getSelectionStart();
    int end = getTextContents().getSelectionEnd();
    while ((start < end) && (Character.isWhitespace(text.charAt(start)))) {
      start++;
    }
    while ((end > start) && (Character.isWhitespace(text.charAt(end - 1)))) {
      end--;
    }
    if (end <= start) {
      JOptionPane.showMessageDialog(
          getParentComponent(),
          GT._("You must select the text that is written in a foreign language"),
          Version.PROGRAM, JOptionPane.WARNING_MESSAGE);
      return;
    }

    // Ask for language
    LanguageSelectionPanel panel = new LanguageSelectionPanel(
        getWikipedia(), text.substring(start, end));
    int result = JOptionPane.showConfirmDialog(
        getParentComponent(), panel, GT._("Foreign language"),
        JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
    if (result != JOptionPane.OK_OPTION) {
      return;
    }
    String language = panel.getLanguage();
    if ((language == null) || (language.length() == 0)) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    config.setString(null, ConfigurationValueString.LAST_LANGUAGE, panel.getLang());

    // Mark text
    StringBuilder newText = new StringBuilder();
    if (start > getTextContents().getSelectionStart()) {
      newText.append(text.substring(getTextContents().getSelectionStart(), start));
    }
    newText.append("{{");
    newText.append(elements[0]);
    newText.append("|");
    if ((elements.length > 1) && (elements[1] != null) && (elements[1].trim().length() > 0)) {
      newText.append(elements[1]);
      newText.append("=");
    }
    newText.append(language);
    newText.append("|");
    if ((elements.length > 2) && (elements[2] != null) && (elements[2].trim().length() > 0)) {
      newText.append(elements[2]);
      newText.append("=");
    }
    newText.append(text.substring(start, end));
    newText.append("}}");
    if (getTextContents().getSelectionEnd() > end) {
      newText.append(text.substring(end, getTextContents().getSelectionEnd()));
    }
    getTextContents().replaceSelection(newText.toString());
  }

  /**
   * Action called when Translate button is pressed.
   */
  public void actionTranslate() {
    Object from = Utilities.askForValue(
        getParentComponent(),
        GT._("From which Wikipedia is this text coming ?"),
        EnumWikipedia.values(), getWikipedia());
    if ((from == null) || (from == getWikipedia())) {
      return;
    }
    if (!(from instanceof EnumWikipedia)) {
      return;
    }
    TranslateWorker worker = new TranslateWorker(
        getWikipedia(), this, (EnumWikipedia) from,
        getPage(), getTextContents().getText());
    worker.setListener(new DefaultBasicWorkerListener() {

      @Override
      public void afterFinished(BasicWorker localWorker, boolean ok) {
        if (!ok) {
          return;
        }
        Object result = localWorker.get();
        if ((result == null) || !(result instanceof String)) {
          return;
        }
        getTextContents().changeText((String) result);
        translated = true;
        actionValidate(true);
      }
      
    });
    worker.start();
  }

  /**
   * Count pages occurrences.
   * 
   * @param analysis Page analysis.
   * @param forceDisambiguation Flag indicating if disambiguation should be counted.
   */
  void countOccurrences(PageAnalysis analysis, boolean forceDisambiguation) {
    Page page = getPage();
    if ((page != null) && (page.getLinks() != null)) {
      List<Page> links = new ArrayList<Page>();
      for (Page link : page.getLinks()) {
        if (link != null) {
          boolean count = false;
          if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
            if (forceDisambiguation || modelLinks.getCountDisambiguation()) {
              count = true;
            }
          } else if (link.isRedirect()) {
            if (modelLinks.getCountRedirect()) {
              count = true;
            }
          } else if (Boolean.FALSE.equals(link.isExisting())) {
            if (modelLinks.getCountMissing()) {
              count = true;
            }
          } else {
            if (modelLinks.getCountOther()) {
              count = true;
            }
          }
          if (count) {
            links.add(link);
          }
        }
      }
      analysis.countLinks(links);
      listCellRenderer.setPageAnalysis(analysis);
      modelLinks.setPageAnalysis(analysis);
    }
  }

  /**
   * Contributions.
   */
  private Contributions contributions;

  /**
   * @return Contributions.
   */
  @Override
  public Contributions getContributions() {
    return contributions;
  }

  /**
   * @param analysis Page analysis.
   * @return Default comment.
   */
  @Override
  protected String getAutomaticComment(PageAnalysis analysis) {

    // Comment for translation
    WPCConfiguration configuration = getConfiguration();
    if (translated) {
      String text = configuration.getString(WPCConfigurationString.TRANSLATION_COMMENT);
      if ((text != null) && (text.trim().length() > 0)) {
        return text;
      }
      return GT._("Translation");
    }

    contributions = new Contributions(getWikipedia());
    contributions.increasePages(1);

    // Comment for fixed links to disambiguation pages
    List<String> dabLinks = new ArrayList<String>();
    StringBuilder comment = new StringBuilder();
    if ((mapLinksTotalCount != null) && (mapLinksTotalCount.size() > 0)) {
      List<String> fixed = new ArrayList<String>();
      List<String> helpRequested = new ArrayList<String>();
      for (Entry<String, Integer> p : mapLinksTotalCount.entrySet()) {
        if ((p != null) && (p.getKey() != null) && (p.getValue() != null)) {
          Integer currentCount = null;
          Integer currentHelpCount = null;
          Page page = getPage();
          if ((page != null) && (page.getLinks() != null)) {
            for (Page link : page.getLinks()) {
              if (Page.areSameTitle(p.getKey(), link.getTitle())) {
                InternalLinkCount count = analysis.getLinkCount(link);
                if (count != null) {
                  currentCount = Integer.valueOf(count.getTotalLinkCount());
                  currentHelpCount = Integer.valueOf(count.getHelpNeededCount());
                }
              }
            }
          }
          if ((currentCount == null) || (currentCount < p.getValue().intValue())) {
            fixed.add(p.getKey());
          } else {
            Integer helpCount = mapLinksHelpNeededCount.get(p.getKey());
            if ((helpCount != null) &&
                ((currentHelpCount == null) || (currentHelpCount > helpCount.intValue()))) {
              helpRequested.add(p.getKey());
            }
          }
        }
      }

      // Compute list of disambiguation links that still need to be fixed
      List<Page> links = analysis.getPage().getLinks();
      if (links != null) {
        analysis.countLinks(links);
        for (Page link : links) {
          if (Boolean.TRUE.equals(link.isDisambiguationPage())) {
            InternalLinkCount linkCount = analysis.getLinkCount(link);
            if (linkCount != null) {
              if ((linkCount.getInternalLinkCount() > 0) ||
                  (linkCount.getIncorrectTemplateCount() > 0) ||
                  (linkCount.getHelpNeededCount() > 0)) {
                dabLinks.add(link.getTitle());
              }
            }
          }
        }
      }

      // Add comment
      boolean showDabLinks = false;
      if (fixed.size() > 0) {
        Collections.sort(fixed);
        contributions.increaseDabLinks(fixed.size());
        comment.append(configuration.getDisambiguationComment(
            fixed.size(), fixed));
        showDabLinks = true;
      } else if (helpRequested.size() > 0) {
        Collections.sort(helpRequested);
        comment.append(configuration.getDisambiguationCommentHelp(
            helpRequested.size(), helpRequested));
        showDabLinks = true;
      }
      if (showDabLinks) {
        if (dabLinks.size() > 0) {
          Collections.sort(dabLinks);
          comment.append(configuration.getDisambiguationCommentTodo(
              dabLinks.size(), dabLinks));
          dabLinks.clear();
        }
      }
    }

    // Comment for fixed Check Wiki errors
    if ((getInitialErrors() != null) && (getInitialErrors().size() > 0)) {
      List<CheckErrorAlgorithm> errorsFixed = computeErrorsFixed();
      if ((errorsFixed != null) && (errorsFixed.size() > 0)) {
        if (comment.length() > 0) {
          comment.append(" / ");
        }
        comment.append(getWikipedia().getCWConfiguration().getComment());
        Configuration config = Configuration.getConfiguration();
        for (CheckErrorAlgorithm errorFixed : errorsFixed) {
          contributions.increaseCheckWikiError(errorFixed.getErrorNumber(), 1);
          comment.append(" - ");
          String link = errorFixed.getLink();
          if ((link != null) &&
              (config != null) &&
              (config.getBoolean(
                  null,
                  ConfigurationValueBoolean.CHECK_LINK_ERRORS))) {
            comment.append("[[");
            comment.append(link);
            comment.append("|");
            comment.append(errorFixed.getShortDescriptionReplaced());
            comment.append("]]");
          } else {
            comment.append(errorFixed.getShortDescriptionReplaced());
          }
        }
      }
    }

    // Comments for added categories / templates
    boolean isCategoryAdded = false;
    if (addedCategories != null) {
      for (PageElementCategory category : analysis.getCategories()) {
        for (String category2 : addedCategories) {
          if (Page.areSameTitle(category.getName(), category2)) {
            isCategoryAdded = true;
          }
        }
      }
    }
    boolean isTemplateAdded = false;
    if (addedTemplates != null) {
      for (PageElementTemplate template : analysis.getTemplates()) {
        for (String template2 : addedTemplates) {
          if (Page.areSameTitle(template.getTemplateName(), template2)) {
            isTemplateAdded = true;
          }
        }
      }
    }
    String strCategoryAdded = configuration.getString(WPCConfigurationString.REDIRECT_CATEGORIES_COMMENT);
    String strTemplateAdded = configuration.getString(WPCConfigurationString.REDIRECT_TEMPLATES_COMMENT);
    if (strTemplateAdded == null) {
      strTemplateAdded = strCategoryAdded;
    }
    if ((isCategoryAdded && (strCategoryAdded != null)) ||
        (isTemplateAdded && (strTemplateAdded != null))) {
      if (comment.length() > 0) {
        comment.append(" / ");
      }
      if (isCategoryAdded && (strCategoryAdded != null)) {
        comment.append(strCategoryAdded);
        if (isTemplateAdded && !strCategoryAdded.equals(strTemplateAdded)) {
          comment.append(" - ");
          comment.append(strCategoryAdded);
        }
      } else {
        comment.append(strTemplateAdded);
      }
    }

    // Comment for disambiguation links
    if ((dabLinks.size() > 0) && (comment.length() > 0)) {
      String newComment = configuration.getDisambiguationWarningComment(dabLinks);
      if ((newComment != null) && (newComment.length() > 0)) {
        comment.append(" / ");
        Collections.sort(dabLinks);
        comment.append(newComment);
        dabLinks.clear();
      }
    }

    return comment.toString();
  }

  /**
   * Action called when Validate button is pressed.
   */
  @Override
  protected void actionValidate(boolean fullValidate) {

    // Count previous selected errors
    int previousCount = countSelectedErrors(getPage().getLastAnalysis());

    getTextContents().resetAttributes();
    PageAnalysis analysis = getPage().getAnalysis(getTextContents().getText(), true);
    countOccurrences(analysis, false);
    listLinks.repaint();

    // Check for new errors
    analysis.shouldCheckSpelling(shouldCheckSpelling());
    List<CheckErrorPage> errorsFound = CheckError.analyzeErrors(
        allAlgorithms, analysis);
    if (errorsFound != null) {
      for (CheckErrorPage tmpError : errorsFound) {
        boolean errorFound = false;
        for (int index = 0; index < modelErrors.getSize(); index++) {
          CheckErrorPage errorModel = (CheckErrorPage) modelErrors.get(index);
          if ((errorModel != null) &&
              (errorModel.getAlgorithm() != null) &&
              (errorModel.getAlgorithm().equals(tmpError.getAlgorithm()))) {
            errorFound = true;
            modelErrors.set(index, tmpError);
          }
        }
        if (!errorFound) {
          modelErrors.addElement(tmpError);
        }
      }
    }
    for (int index = 0; index < modelErrors.getSize(); index++) {
      CheckErrorPage errorModel = (CheckErrorPage) modelErrors.get(index);
      if ((errorsFound == null) || (!errorsFound.contains(errorModel))) {
        CheckErrorPage newError = new CheckErrorPage(getPage(), errorModel.getAlgorithm());
        modelErrors.set(index, newError);
      }
    }
    listErrors.repaint();

    // Update comment 
    setComment(getAutomaticComment(analysis));

    // Update selection
    if (fullValidate) {
      int currentCount = countSelectedErrors(analysis);

      if (listErrors.getSelectedValue() != null) {
        // The previous selection was in the check wiki errors
        CheckErrorPage errorPage = (CheckErrorPage) listErrors.getSelectedValue();
        if (!errorPage.getErrorFound() || (currentCount == previousCount)) {
          int selected = listErrors.getSelectedIndex();
          selected++;
          if (selected < modelErrors.getSize()) {
            listErrors.setSelectedIndex(selected);
          } else {
            if (modelLinks.getSize() > 0) {
              selectLinks(0);
            } else {
              listErrors.setSelectedIndex(0);
            }
          }
        } else {
          getTextContents().resetAttributes();
        }
      } else if (listLinks.getSelectedValue() != null) {
        // The previous selection was in the links
        if ((currentCount == 0) || (currentCount == previousCount)) {
          int selected = listLinks.getMaxSelectionIndex() + 1;
          if (selected < modelLinks.getSize()) {
            selectLinks(selected);
          } else if (modelErrors.getSize() > 0) {
            listErrors.setSelectedIndex(0);
          } else {
            selectLinks(0);
          }
        }
      } else {
        // No previous selection
        if (modelLinks.getSize() > 0) {
          selectLinks(0);
        } else if (modelErrors.getSize() > 0) {
          listErrors.setSelectedIndex(0);
        }
      }
  
      modelLinks.updateLinkCount();
      getTextContents().requestFocusInWindow();
    }
  }

  /**
   * @return Number of errors in the selection.
   */
  private int countSelectedErrors(PageAnalysis analysis) {
    // Count check wiki errors
    if (listErrors.getSelectedValue() != null) {
      CheckErrorPage errorPage = (CheckErrorPage) listErrors.getSelectedValue();
      return errorPage.getResultsCount();
    }

    // Count disambiguation links
    if (listLinks.getSelectedValue() != null) {
      Object[] values = listLinks.getSelectedValues();
      if (values != null) {
        int count = 0;
        for (Object value : values) {
          if (value instanceof Page) {
            InternalLinkCount tmpCount = analysis.getLinkCount((Page) value);
            if (tmpCount != null) {
              count += tmpCount.getTotalLinkCount();
            }
          }
        }
        return count;
      }
    }
    return 0;
  }

  /**
   * Action called when Delete button is pressed.
   */
  public void actionDelete() {
    String reason = askForValue(
        GT._("Do you want to delete this page on Wikipedia ?\nPlease, enter the reason for deleting the page"),
        "", null);
    if ((reason == null) || (reason.trim().length() == 0)) {
      return;
    }
    API api = APIFactory.getAPI();
    try {
      api.deletePage(getWikipedia(), getPage(), getWikipedia().formatComment(reason.trim(), false));
      dispose();
    } catch (APIException e) {
      displayError(e);
    }
  }
}
