/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
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
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.gui.swing.action.SetComparatorAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.AbstractPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.AnalysisPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.CheckErrorPageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.CheckErrorPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.MediaWikiConstants;
import org.wikipediacleaner.gui.swing.component.PageListAnalyzeListener;
import org.wikipediacleaner.gui.swing.component.PageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.worker.FullAnalysisWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;


/**
 * Analysis window.
 */
public class AnalysisWindow extends PageWindow {

  private final static String ACTION_DISAMBIGUATION_LINK    = "DISAMBIGUATION LINK";
  private final static String ACTION_FULL_ANALYSIS_LINK     = "FULL ANALYSIS LINK";
  private final static String ACTION_WATCH_LINK             = "WATCH LINK";
  private final static String ACTION_DISAMBIGUATION_WARNING = "DISAMBIGUATION WARNING";

  private JButton buttonFirst;
  private JButton buttonPrevious;
  private JButton buttonNext;
  private JButton buttonLast;
  private JButton buttonToc;
  private JButton buttonValidate;

  JList listLinks;
  PageListModel modelLinks;
  Map<String, Integer> mapLinksCount;
  private AbstractPageListPopupListener popupListenerLinks;
  JCheckBoxMenuItem menuItemShowDisambiguation;
  JCheckBoxMenuItem menuItemShowMissing;
  JCheckBoxMenuItem menuItemShowOther;
  JCheckBoxMenuItem menuItemShowRedirect;
  JCheckBoxMenuItem menuItemCountDisambiguation;
  JCheckBoxMenuItem menuItemCountMissing;
  JCheckBoxMenuItem menuItemCountOther;
  JCheckBoxMenuItem menuItemCountRedirect;
  private JButton buttonFullAnalysisLink;
  private JButton buttonDisambiguationLink;
  private JButton buttonWatchLink;
  private JButton buttonDisambiguationWarning;

  List<CheckErrorAlgorithm> allAlgorithms;
  JList listErrors;
  private DefaultListModel modelErrors;

  List<Page> knownPages;

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
        AnalysisWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof AnalysisWindow) {
              Configuration config = Configuration.getConfiguration();
              AnalysisWindow analysis = (AnalysisWindow) window;
              analysis.setPageName(page);
              analysis.knownPages = knownPages;
              analysis.modelLinks = new PageListModel();
              analysis.modelLinks.setShowDisambiguation(config.getBoolean(
                  null,
                  Configuration.BOOLEAN_ANALYSIS_DISAMBIG_PAGES,
                  Configuration.DEFAULT_ANALYSIS_DISAMBIG_PAGES));
              analysis.modelLinks.setShowMissing(config.getBoolean(
                  null,
                  Configuration.BOOLEAN_ANALYSIS_MISSING_PAGES,
                  Configuration.DEFAULT_ANALYSIS_MISSING_PAGES));
              analysis.modelLinks.setShowOther(config.getBoolean(
                  null,
                  Configuration.BOOLEAN_ANALYSIS_OTHER_PAGES,
                  Configuration.DEFAULT_ANALYSIS_OTHER_PAGES));
              analysis.modelLinks.setShowRedirect(config.getBoolean(
                  null,
                  Configuration.BOOLEAN_ANALYSIS_REDIRECT_PAGES,
                  Configuration.DEFAULT_ANALYSIS_REDIRECT_PAGES));
              analysis.modelLinks.setCountDisambiguation(config.getBoolean(
                  null,
                  Configuration.BOOLEAN_ANALYSIS_COUNT_DISAMBIG,
                  Configuration.DEFAULT_ANALYSIS_COUNT_DISAMBIG));
              analysis.modelLinks.setCountMissing(config.getBoolean(
                  null,
                  Configuration.BOOLEAN_ANALYSIS_COUNT_MISSING,
                  Configuration.DEFAULT_ANALYSIS_COUNT_MISSING));
              analysis.modelLinks.setCountOther(config.getBoolean(
                  null,
                  Configuration.BOOLEAN_ANALYSIS_COUNT_OTHER,
                  Configuration.DEFAULT_ANALYSIS_COUNT_OTHER));
              analysis.modelLinks.setCountRedirect(config.getBoolean(
                  null,
                  Configuration.BOOLEAN_ANALYSIS_COUNT_REDIRECT,
                  Configuration.DEFAULT_ANALYSIS_COUNT_REDIRECT));
              analysis.modelLinks.setComparator(PageComparator.getNamespaceFirstComparator());
              analysis.createTextContents(window);
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof AnalysisWindow) {
              AnalysisWindow analysis = (AnalysisWindow) window;
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
    buttonFirst.setEnabled(isPageLoaded());
    buttonPrevious.setEnabled(isPageLoaded());
    buttonNext.setEnabled(isPageLoaded());
    buttonLast.setEnabled(isPageLoaded());
    buttonToc.setEnabled(isPageLoaded());
    buttonValidate.setEnabled(isPageLoaded());
    buttonDisambiguationWarning.setEnabled(article);
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
    buttonFirst = createButtonFirstOccurence(this, true);
    toolbarButtons.add(buttonFirst);
    buttonPrevious = createButtonPreviousOccurence(this, true);
    toolbarButtons.add(buttonPrevious);
    buttonNext = createButtonNextOccurence(this, true);
    toolbarButtons.add(buttonNext);
    buttonLast = createButtonLastOccurence(this, true);
    toolbarButtons.add(buttonLast);
    toolbarButtons.addSeparator();
    addButtonUndoRedo(toolbarButtons, true);
    buttonToc = createButtonToc(this, true);
    toolbarButtons.add(buttonToc);
    buttonValidate = createButtonValidate(this, true);
    toolbarButtons.add(buttonValidate);
    addButtonSend(toolbarButtons, true);
    addButtonRedirect(toolbarButtons);
    buttonDisambiguationWarning = Utilities.createJButton(
        "gnome-dialog-warning.png", EnumImageSize.NORMAL,
        GT._("Add a warning on the talk page about the links to disambiguation pages"),
        false); 
    buttonDisambiguationWarning.setActionCommand(ACTION_DISAMBIGUATION_WARNING);
    buttonDisambiguationWarning.addActionListener(this);
    toolbarButtons.add(buttonDisambiguationWarning);
    toolbarButtons.addSeparator();
    addButtonReload(toolbarButtons, true);
    addButtonView(toolbarButtons, true);
    addButtonViewHistory(toolbarButtons, true);
    toolbarButtons.addSeparator();
    addButtonWatch(toolbarButtons, true);
    addButtonDisambiguation(toolbarButtons, true);
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
      JMenuItem menuItem = Utilities.createJMenuItem(comparator.getName());
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
    buttonFullAnalysisLink = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("Full analysis (Alt + &F)"), false);
    buttonFullAnalysisLink.setActionCommand(ACTION_FULL_ANALYSIS_LINK);
    buttonFullAnalysisLink.addActionListener(this);
    toolbar.add(buttonFullAnalysisLink);
    buttonDisambiguationLink = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Disambiguation (Alt + &D)"), false);
    buttonDisambiguationLink.setActionCommand(ACTION_DISAMBIGUATION_LINK);
    buttonDisambiguationLink.addActionListener(this);
    toolbar.add(buttonDisambiguationLink);
    buttonWatchLink = Utilities.createJButton(
        "gnome-logviewer-add.png", EnumImageSize.NORMAL,
        GT._("Add to Watch list (Alt + &W)"), false);
    buttonWatchLink.setActionCommand(ACTION_WATCH_LINK);
    buttonWatchLink.addActionListener(this);
    toolbar.add(buttonWatchLink);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    panel.add(toolbar, constraints);
    constraints.gridy++;

    // Links
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    listLinks = new JList(modelLinks);
    PageListCellRenderer listCellRenderer = new PageListCellRenderer();
    listCellRenderer.showCountOccurence(true);
    listCellRenderer.showDisambiguation(true);
    listCellRenderer.showMissing(true);
    listCellRenderer.showRedirect(true);
    listLinks.setCellRenderer(listCellRenderer);
    popupListenerLinks = new AnalysisPageListPopupListener(
        getWikipedia(), getTextContents(), this);
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
   * Action called when an error is selected in the list. 
   */
  void actionSelectError(CheckErrorPage errorSelected) {
    boolean modified = getTextContents().isModified();
    String contents = getTextContents().getText();
    PageAnalysis pageAnalysis = new PageAnalysis(errorSelected.getPage(), contents);
    CheckErrorPage errorPage = CheckError.analyzeError(
        errorSelected.getAlgorithm(), pageAnalysis);
    getTextContents().resetAttributes();
    StyledDocument document = getTextContents().getStyledDocument();
    if (document != null) {
      if (errorPage.getResults() != null) {
        for (CheckErrorResult errorFound : errorPage.getResults()) {
          String styleName = MediaWikiConstants.STYLE_CHECK_WIKI_ERROR;
          if (errorFound.getErrorLevel() == CheckErrorResult.ErrorLevel.CORRECT) {
            styleName = MediaWikiConstants.STYLE_CHECK_WIKI_OK;
          } else if (errorFound.getErrorLevel() == CheckErrorResult.ErrorLevel.WARNING) {
            styleName = MediaWikiConstants.STYLE_CHECK_WIKI_WARNING;
          }
          document.setCharacterAttributes(
              errorFound.getStartPosition(),
              errorFound.getLength(),
              getTextContents().getStyle(styleName),
              true);
          SimpleAttributeSet attributes = new SimpleAttributeSet();
          attributes.addAttribute(MediaWikiConstants.ATTRIBUTE_INFO, errorFound);
          attributes.addAttribute(MediaWikiConstants.ATTRIBUTE_UUID, UUID.randomUUID());
          document.setCharacterAttributes(
              errorFound.getStartPosition(),
              errorFound.getLength(),
              attributes, false);
        }
      }
    }
    listErrors.repaint();
    getTextContents().setModified(modified);
    updateComponentState();
    actionFirstOccurence();
  }

  /**
   * A ListSelectionListener implementation. 
   */
  class AnalysisListSelectionListener implements ListSelectionListener {

    public void valueChanged(ListSelectionEvent e) {
      if (e.getSource() instanceof JList) {
        JList list = (JList) e.getSource();

        if (list == listLinks) {
          // List of links
          Object[] selection = list.getSelectedValues();
          if ((selection != null) && (selection.length > 0)) {
            listErrors.clearSelection();
            List<Page> pages = new ArrayList<Page>();
            for (int i = 0; i < selection.length; i++) {
              if (selection[i] instanceof Page) {
                pages.add((Page) selection[i]);
              }
            }
            List<Page> oldPages = getTextContents().getInternalLinks();
            if (!pages.equals(oldPages)) {
              getTextContents().setInternalLinks(pages);
            }
          } else {
            getTextContents().resetAttributes();
          }
        } else if (list == listErrors) {
          // List of errors
          Object selection = list.getSelectedValue();
          if ((selection != null) && (selection instanceof CheckErrorPage)) {
            listLinks.clearSelection();
            getTextContents().setInternalLinks(null);
            CheckErrorPage errorSelected = (CheckErrorPage) selection;
            actionSelectError(errorSelected);
          } else {
            getTextContents().resetAttributes();
          }
        }
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

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    super.actionPerformed(e);
    if (ACTION_WATCH_LINK.equals(e.getActionCommand())) {
      actionWatchLink();
    } else if (ACTION_DISAMBIGUATION_LINK.equals(e.getActionCommand())) {
      actionDisambiguationLink();
    } else if (ACTION_FULL_ANALYSIS_LINK.equals(e.getActionCommand())) {
      actionFullAnalysisLink();
    } else if (ACTION_FIRST_OCCURENCE.equals(e.getActionCommand())) {
      actionFirstOccurence();
    } else if (ACTION_PREVIOUS_OCCURENCE.equals(e.getActionCommand())) {
      actionPreviousOccurence();
    } else if (ACTION_NEXT_OCCURENCE.equals(e.getActionCommand())) {
      actionNextOccurence();
    } else if (ACTION_LAST_OCCURENCE.equals(e.getActionCommand())) {
      actionLastOccurence();
    } else if (ACTION_DISAMBIGUATION_WARNING.equals(e.getActionCommand())) {
      actionDisambiguationWarning();
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
    FullAnalysisWorker reloadWorker = new FullAnalysisWorker(getWikipedia(), this, getPage(), knownPages);
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
    mapLinksCount = new HashMap<String, Integer>();
    if ((page != null) && (page.getLinks() != null)) {
      List<Page> links = page.getLinks();
      modelLinks.setElements(links);
      countOccurences(page.getContents(), true);
      for (Page p : links) {
        if ((p.isDisambiguationPage()) && (p.getCountOccurence() > 0)) {
          mapLinksCount.put(p.getTitle(), Integer.valueOf(p.getCountOccurence()));
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
      boolean isArticle = (getPage() != null) && (getPage().isArticle());
      if (isArticle) {
        Configuration config = Configuration.getConfiguration();
        if (getPage().isInMainNamespace()) {
          chkUpdateDabWarning.setSelected(config.getBoolean(
              null,
              Configuration.BOOLEAN_UPDATE_DAB_WARNING,
              Configuration.DEFAULT_UPDATE_DAB_WARNING));
          chkCreateDabWarning.setSelected(config.getBoolean(
              null,
              Configuration.BOOLEAN_CREATE_DAB_WARNING,
              Configuration.DEFAULT_CREATE_DAB_WARNING));
        } else if ((getPage().getNamespace() != null) &&
                   (getWikipedia().isEncyclopedicNamespace(getPage().getNamespace()))) {
          chkUpdateDabWarning.setSelected(config.getBoolean(
              null,
              Configuration.BOOLEAN_UPDATE_DAB_WARNING_ENCY,
              Configuration.DEFAULT_UPDATE_DAB_WARNING_ENCY));
          chkCreateDabWarning.setSelected(config.getBoolean(
              null,
              Configuration.BOOLEAN_CREATE_DAB_WARNING_ENCY,
              Configuration.DEFAULT_CREATE_DAB_WARNING_ENCY));
        } else {
          chkUpdateDabWarning.setSelected(config.getBoolean(
              null,
              Configuration.BOOLEAN_UPDATE_DAB_WARNING_ALL,
              Configuration.DEFAULT_UPDATE_DAB_WARNING_ALL));
          chkCreateDabWarning.setSelected(config.getBoolean(
              null,
              Configuration.BOOLEAN_CREATE_DAB_WARNING_ALL,
              Configuration.DEFAULT_CREATE_DAB_WARNING_ALL));
        }
      }
    }
    updateComponentState();

    // Automatic fix of some errors
    if ((getInitialErrors() != null) && (getTextContents() != null)) {
      String initialContents = getTextContents().getText();
      String contents = initialContents;
      for (CheckErrorPage error : getInitialErrors()) {
        contents = error.getAlgorithm().automaticFix(getPage(), contents);
      }
      if (!contents.equals(initialContents)) {
        getTextContents().changeText(contents);
        actionValidate();
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
        Configuration.INTEGER_ANALYSIS_NB_PAGES,
        Configuration.DEFAULT_ANALYSIS_NB_PAGES);
    int nbSelections = Math.max(Math.min(maxSelections, modelLinks.getSize() - startIndex), 1);
    startIndex = Math.min(startIndex, modelLinks.getSize() - nbSelections);
    listLinks.getSelectionModel().setSelectionInterval(startIndex, startIndex + nbSelections - 1);
    listLinks.ensureIndexIsVisible(startIndex);
    listLinks.ensureIndexIsVisible(startIndex + nbSelections - 1);
  }

  /**
   * Action called when Watch link button is pressed. 
   */
  private void actionWatchLink() {
    Object[] links = listLinks.getSelectedValues();
    if ((links == null) || (links.length == 0)) {
      return;
    }
    if (displayYesNoWarning(
        GT._("Would you like to add these pages on your local Watch list ?")) == JOptionPane.YES_OPTION) {
      Configuration config = Configuration.getConfiguration();
      List<String> watch = config.getStringList(getWikipedia(), Configuration.ARRAY_WATCH_PAGES);
      boolean added = false;
      for (Object link : links) {
        if (!watch.contains(link.toString())) {
          added = true;
          watch.add(link.toString());
        }
      }
      if (added) {
        Collections.sort(watch);
        config.setStringList(getWikipedia(), Configuration.ARRAY_WATCH_PAGES, watch);
      }
    }
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  private void actionDisambiguationLink() {
    Controller.runDisambiguationAnalysis(
        getParentComponent(),
        listLinks.getSelectedValues(),
        getWikipedia());
  }

  /**
   * Action called when Disambiguation warning button is pressed.  
   */
  private void actionDisambiguationWarning() {
    String template = getWikipedia().getDisambiguationWarningTemplate();
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You need to define the 'dab_warning_template' property in WikiCleaner configuration."));
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
        true, true, true);
    worker.start();
  }

  /**
   * Action called when Full analysis button is pressed.
   */
  private void actionFullAnalysisLink() {
    Controller.runFullAnalysis(
        getParentComponent(),
        listLinks.getSelectedValues(),
        null,
        getWikipedia());
  }

  /**
   * Count pages occurences.
   * 
   * @param text Page text.
   * @param forceDisambiguation Flag indicating if disambiguation should be counted.
   */
  void countOccurences(String text, boolean forceDisambiguation) {
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
      PageContents.countInternalLinks(getWikipedia(), page, text, links);
    }
  }

  /**
   * Action called when Validate button is pressed.
   */
  @Override
  protected void actionValidate() {
    getTextContents().resetAttributes();
    countOccurences(getTextContents().getText(), false);
    listLinks.repaint();

    // Check for new errors
    PageAnalysis pageAnalysis = new PageAnalysis(getPage(), getTextContents().getText());
    List<CheckErrorPage> errorsFound = CheckError.analyzeErrors(
        allAlgorithms, pageAnalysis);
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
    StringBuilder comment = new StringBuilder();
    if ((mapLinksCount != null) && (mapLinksCount.size() > 0)) {
      // Comment for fixed links to disambiguation pages
      List<String> fixed = new ArrayList<String>();
      for (Entry<String, Integer> p : mapLinksCount.entrySet()) {
        if ((p != null) && (p.getKey() != null) && (p.getValue() != null)) {
          Integer currentCount = null;
          Page page = getPage();
          if ((page != null) && (page.getLinks() != null)) {
            for (Page link : page.getLinks()) {
              if (Page.areSameTitle(p.getKey(), link.getTitle())) {
                currentCount = link.getCountOccurence();
              }
            }
          }
          if ((currentCount == null) || (currentCount < p.getValue().intValue())) {
            fixed.add(p.getKey());
          }
        }
      }
      if (fixed.size() > 0) {
        Collections.sort(fixed);
        comment.append(getDefaultComment());
        int linksFixed = 0;
        for (String fix : fixed) {
          if (linksFixed > 0) {
            comment.append(", ");
          } else {
            comment.append(" - ");
          }
          linksFixed++;
          comment.append("[[" + fix + "]]");
        }
      }
    }
    if ((getInitialErrors() != null) && (getInitialErrors().size() > 0)) {
      // Comment for fixed Check Wiki errors
      List<CheckErrorAlgorithm> errorsFixed = computeErrorsFixed();
      if ((errorsFixed != null) && (errorsFixed.size() > 0)) {
        if (comment.length() > 0) {
          comment.append(" / ");
        }
        comment.append(GT._("Detection by [[{0}]]", getWikipedia().getCheckWikiProject()));
        Configuration config = Configuration.getConfiguration();
        for (CheckErrorAlgorithm errorFixed : errorsFixed) {
          comment.append(" - ");
          String link = errorFixed.getLink();
          if ((link != null) &&
              (config != null) &&
              (config.getBoolean(
                  null,
                  Configuration.BOOLEAN_CHECK_LINK_ERRORS,
                  Configuration.DEFAULT_CHECK_LINK_ERRORS))) {
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
    setComment(comment.toString());

    // If the selected links are fixed, select the next one
    if (listErrors.getSelectedValue() != null) {
      CheckErrorPage errorPage = (CheckErrorPage) listErrors.getSelectedValue();
      if (!errorPage.getErrorFound()) {
        int selected = listErrors.getSelectedIndex();
        selected++;
        if (selected < modelErrors.getSize()) {
          listErrors.setSelectedIndex(selected);
        } else {
          listErrors.setSelectedIndex(0);
        }
      } else {
        actionSelectError(errorPage);
      }
    } else {
      Object[] values = listLinks.getSelectedValues();
      if ((values != null) && (values.length > 0)) {
        int count = 0;
        int countElement = 0;
        for (Object value : values) {
          if (value instanceof Page) {
            countElement++;
            count += ((Page) value).getCountOccurence();
          }
        }
        if ((countElement > 0) && (count == 0)) {
          int selected = listLinks.getMaxSelectionIndex();
          selected++;
          if (selected < modelLinks.getSize()) {
            selectLinks(selected);
          }
        }
      }
    }

    modelLinks.updateLinkCount();
    getTextContents().requestFocusInWindow();
  }

  /**
   * Action called when First Occurence button is pressed. 
   */
  void actionFirstOccurence() {
    getTextContents().selectFirstOccurence();
    getTextContents().requestFocusInWindow();
  }

  /**
   * Action called when Previous Occurence button is pressed. 
   */
  private void actionPreviousOccurence() {
    getTextContents().selectPreviousOccurence();
    getTextContents().requestFocusInWindow();
  }

  /**
   * Action called when Next Occurence button is pressed. 
   */
  private void actionNextOccurence() {
    getTextContents().selectNextOccurence();
    getTextContents().requestFocusInWindow();
  }

  /**
   * Action called when Last Occurence button is pressed. 
   */
  private void actionLastOccurence() {
    getTextContents().selectLastOccurence();
    getTextContents().requestFocusInWindow();
  }
}
