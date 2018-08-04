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
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.action.ActionDisambiguationAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.gui.swing.action.ActionFullAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionWatchPage;
import org.wikipediacleaner.gui.swing.action.SetComparatorAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.DisambiguationPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.PageListAnalyzeListener;
import org.wikipediacleaner.gui.swing.component.PageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.worker.DisambiguationAnalysisWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * Disambiguation window.
 */
public class DisambiguationWindow extends OnePageWindow {

  //public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  private Properties backlinksProperties;

  JList<Page> listLinks;
  PageListModel modelLinks;
  private PageListCellRenderer listCellRenderer;
  private DisambiguationPageListPopupListener popupListenerLinks;
  JLabel linkCount;

  private JButton buttonAutomaticFixing;
  private JButton buttonFullAnalysisLink;
  private JButton buttonMarkNeedHelp;
  private JButton buttonMarkNormal;
  private JButton buttonSelectNextLinks;
  private JButton buttonView;
  private JButton buttonViewHistory;
  private JButton buttonViewLink;
  private JButton buttonWatch;

  private JMenu menuFilter;

  List<Page> knownPages;

  /**
   * Create and display a DisambiguationWindow.
   * 
   * @param page Page name.
   * @param wikipedia Wikipedia.
   */
  public static void createDisambiguationWindow(
      final String page,
      final EnumWikipedia wikipedia) {
    createWindow(
        "DisambiguationWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        DisambiguationWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof DisambiguationWindow) {
              DisambiguationWindow disambig = (DisambiguationWindow) window;
              disambig.setPageName(page);
              disambig.modelLinks = new PageListModel();
              disambig.modelLinks.setComparator(PageComparator.getTemplateFirstComparator());
              disambig.modelLinks.setShowDisambiguation(true);
              disambig.modelLinks.setShowOther(true);
              Configuration config = Configuration.getConfiguration();
              List<String> filtered = config.getStringList(wikipedia, Configuration.ARRAY_FILTER_NS);
              disambig.modelLinks.setFilterNamespace(filtered);
              disambig.knownPages = new ArrayList<Page>();
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof DisambiguationWindow) {
              DisambiguationWindow disambig = (DisambiguationWindow) window;
              disambig.actionReload();
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._T("Disambiguation - {0}", getPageName());
  }

  /**
   * @return Menu bar.
   */
  @Override
  protected JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    menuBar.add(createToolsMenu());
    menuBar.add(createSortMenu());
    menuBar.add(Box.createHorizontalGlue());
    return menuBar;
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    createElements();
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
    JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    split.setLeftComponent(createLinksComponents());
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
   * Update component state.
   */
  @Override
  protected void updateComponentState() {
    super.updateComponentState();
    setEnabledStatus(buttonFullAnalysisLink, isPageLoaded());
    setEnabledStatus(buttonView, isPageLoaded());
    setEnabledStatus(buttonViewHistory, isPageLoaded());
    setEnabledStatus(buttonViewLink, isPageLoaded());
    setEnabledStatus(buttonWatch, isPageLoaded());
  }

  /**
   * Creates internal elements. 
   */
  private void createElements() {
    createTextContents(this);
  }

  /**
   * @return Page components.
   */
  private Component createPageComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    addTextPageName(panel);
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    toolbar.setBorderPainted(false);
    addButtonReload(toolbar, true);
    buttonView = ActionExternalViewer.addButton(
        toolbar, getWikipedia(), getPageName(), false, true, false);
    buttonViewHistory = ActionExternalViewer.addButton(
        toolbar, getWikipedia(), getPageName(), ActionExternalViewer.ACTION_HISTORY, true, true);
    addButtonSend(toolbar, true);
    buttonWatch = ActionWatchPage.addButton(
        getParentComponent(), toolbar, getWikipedia(), getPageName(), true, true);
    addButtonFullAnalysis(toolbar, true);
    panel.add(toolbar);
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

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    addTextContents(panel, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Links components.
   */
  private Component createLinksComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    Configuration configuration = Configuration.getConfiguration();

    listLinks = new JList<Page>(modelLinks);

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

    // Select next links button
    buttonSelectNextLinks = Utilities.createJButton(GT._T("Select &next links"), null);
    buttonSelectNextLinks.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSelectNextLinks"));
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    panel.add(buttonSelectNextLinks, constraints);
    constraints.gridy++;

    // Automatic fixing
    buttonAutomaticFixing = Utilities.createJButton(GT._T("Automatic fixing"), null);
    buttonAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRunAutomaticFixing"));
    panel.add(buttonAutomaticFixing, constraints);
    constraints.gridy++;

    // Button toolbar
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    toolbar.setBorderPainted(false);
    buttonFullAnalysisLink = ActionFullAnalysis.addButton(
        getParentComponent(), toolbar, getWikipedia(), listLinks, knownPages, true, true);
    ActionDisambiguationAnalysis.addButton(
        getParentComponent(), toolbar, getWikipedia(), listLinks, true, true);
    buttonMarkNormal = Utilities.createJButton(
        "wpc-mark-normal.png", EnumImageSize.NORMAL,
        GT._T("Mark backlink as normal"), false, null);
    buttonMarkNormal.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionMarkBacklinkNormal"));
    toolbar.add(buttonMarkNormal);
    buttonMarkNeedHelp = Utilities.createJButton(
        "wpc-mark-need-help.png", EnumImageSize.NORMAL,
        GT._T("Mark backlink as needing help"), false, null);
    buttonMarkNeedHelp.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionMarkBacklinkHelpNeeded"));
    toolbar.add(buttonMarkNeedHelp);
    buttonViewLink = ActionExternalViewer.addButton(
        toolbar, getWikipedia(), listLinks, false, true, true);
    toolbar.addSeparator();
    linkCount = new JLabel(GT._T("Link count"));
    toolbar.add(linkCount);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(toolbar, constraints);
    constraints.gridy++;

    // Links
    listCellRenderer = new PageListCellRenderer();
    listCellRenderer.showRedirect(true);
    listCellRenderer.showRedirectBacklinks(true);
    if (getPage() != null) {
      listCellRenderer.setPageProperties(configuration.getSubProperties(
          getWikipedia(), Configuration.PROPERTIES_BACKLINKS, getPage().getTitle()));
    }
    listLinks.setCellRenderer(listCellRenderer);
    popupListenerLinks = new DisambiguationPageListPopupListener(
        getWikipedia(), getTextContents(), listLinks, this);
    listLinks.addMouseListener(popupListenerLinks);
    listLinks.addMouseListener(new PageListAnalyzeListener(getWikipedia(), this));
    JScrollPane scrollLinks = new JScrollPane(listLinks);
    scrollLinks.setMinimumSize(new Dimension(100, 100));
    scrollLinks.setPreferredSize(new Dimension(200, 500));
    scrollLinks.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    panel.add(scrollLinks, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Tools menu.
   */
  @Override
  protected JMenu createToolsMenu() {
    JMenu menu = super.createToolsMenu();
    menu.add(createFilterMenu());
    return menu;
  }

  /**
   * @return Filter namespace menu.
   */
  private JMenu createFilterMenu() {
    if (menuFilter == null) {
      menuFilter = Utilities.createJMenu(GT._T("Filter namespaces"));
    } else {
      menuFilter.removeAll();
    }
    EnumWikipedia wiki = getWikipedia();
    if (wiki == null) {
      return menuFilter;
    }
    Configuration config = Configuration.getConfiguration();
    List<String> filtered = config.getStringList(wiki, Configuration.ARRAY_FILTER_NS);
    for (Namespace ns : wiki.getWikiConfiguration().getNamespaces()) {
      Integer id = ns.getId();
      if ((id != null) && (id.intValue() >= 0)) {
        boolean active = !filtered.contains(Integer.toString(ns.getId()));
        JMenuItem item = new JCheckBoxMenuItem(
            ns.toString(), active);
        item.setActionCommand(id.toString());
        item.addActionListener(EventHandler.create(
            ActionListener.class, this,
            active ? "actionFilterNamespaceTrue" : "actionFilterNamespaceFalse",
            "actionCommand"));
        menuFilter.add(item);
      }
    }
    return menuFilter;
  }

  /**
   * Action called to activate the filter on a namespace.
   * 
   * @param namespaceId Namespace identifier.
   */
  public void actionFilterNamespaceTrue(String namespaceId) {
    actionFilterNamespace(namespaceId, true);
  }

  /**
   * Action called to deactivate the filter on a namespace.
   * 
   * @param namespaceId Namespace identifier.
   */
  public void actionFilterNamespaceFalse(String namespaceId) {
    actionFilterNamespace(namespaceId, false);
  }

  /**
   * Action called to activate/deactivate the filter on a namespace.
   * 
   * @param namespaceId Namespace identifier.
   * @param filter True to activate the filter on the namespace.
   */
  private void actionFilterNamespace(String namespaceId, boolean filter) {
    EnumWikipedia wiki = getWikipedia();
    Configuration config = Configuration.getConfiguration();
    List<String> filtered = config.getStringList(wiki, Configuration.ARRAY_FILTER_NS);
    if (filter) {
      filtered.add(namespaceId);
    } else {
      filtered.remove(namespaceId);
    }
    config.setStringList(wiki, Configuration.ARRAY_FILTER_NS, filtered);
    modelLinks.setFilterNamespace(filtered);
  }

  /**
   * @return Sort menu.
   */
  private JMenu createSortMenu() {
    JMenu menu = Utilities.createJMenu(GT._T("Sort"));
    List<CompositeComparator<Page>> comparators = PageComparator.getComparators();
    for (CompositeComparator<Page> comparator : comparators) {
      JMenuItem menuItem = Utilities.createJMenuItem(comparator.getName(), true);
      menuItem.addActionListener(new SetComparatorAction(modelLinks, comparator));
      menu.add(menuItem);
    }
    return menu;
  }

  /**
   * Clean page. 
   */
  @Override
  protected void clean() {
    super.clean();
    modelLinks.clear();
    updateComponentState();
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();
    DisambiguationAnalysisWorker reloadWorker = new DisambiguationAnalysisWorker(
        getWikipedia(), this, getPage());
    setupReloadWorker(reloadWorker);
    reloadWorker.start();
  }

  /**
   * Callback called at the end of the Reload Worker.
   */
  @Override
  protected void afterFinishedReloadWorker() {
    super.afterFinishedReloadWorker();
    Configuration config = Configuration.getConfiguration();
    Page page = getPage();
    backlinksProperties = config.getSubProperties(
        getWikipedia(), Configuration.PROPERTIES_BACKLINKS, page.getTitle());
    listCellRenderer.setPageProperties(backlinksProperties);
    popupListenerLinks.setPage(page);
    popupListenerLinks.setBackLinksProperties(backlinksProperties);
    List<Page> links = page.getAllLinksToPage();
    if (config.getBoolean(null, ConfigurationValueBoolean.IGNORE_DAB_USER_NS)) {
      links = new ArrayList<Page>(links);
      for (int i = links.size(); i > 0; i--) {
        if (links.get(i - 1).isInUserNamespace()) {
          links.remove(i - 1);
        }
      }
    }
    modelLinks.setElements(links);
    Integer countMain = page.getBacklinksCountInMainNamespace();
    Integer countTotal = page.getBacklinksCount();
    linkCount.setText(
        ((countMain != null) ? countMain.toString() : "?") +
        " / " +
        ((countTotal != null) ? countTotal.toString() : "?"));

    // Construct list of known pages
    knownPages.clear();
    if (getPage() != null) {
      knownPages = new ArrayList<Page>(1);
      knownPages.add(getPage());
      for (Page backLink : getPage().getAllLinksToPage()) {
        if ((backLink != null) &&
            (backLink.isRedirect()) &&
            (Page.areSameTitle(getPage().getTitle(), backLink.getRedirectDestination()))) {
          knownPages.add(backLink);
        }
      }
    }

    // Select next links
    actionSelectNextLinks();
  }

  /**
   * Callback called before the start of the Reload Worker. 
   */
  @Override
  protected void beforeStartReloadWorker() {
    super.beforeStartReloadWorker();
    modelLinks.setShowDisambiguation(true);
    modelLinks.setShowOther(true);
  }

  /**
   * Action called when Run Automatic Fixing button is pressed. 
   */
  public void actionRunAutomaticFixing() {
    List<Page> values = listLinks.getSelectedValuesList();
    if ((values == null) || (values.size() == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._T("You must select pages on which running automatic fixing."));
      return;
    }
    Controller.runAutomaticFixing(values, getPage(), getWikipedia());
  }

  /**
   * Action called when Mark back link button is pressed.
   */
  public void actionMarkBacklinkNormal() {
    actionMarkBacklink(Configuration.VALUE_PAGE_NORMAL);
  }

  /**
   * Action called when Mark back link button is pressed.
   */
  public void actionMarkBacklinkHelpNeeded() {
    actionMarkBacklink(Configuration.VALUE_PAGE_HELP_NEEDED);
  }

  /**
   * Action called when Mark back link button is pressed.
   */
  private void actionMarkBacklink(String mark) {
    for (Page selection : listLinks.getSelectedValuesList()) {
      if (selection != null) {
        backlinksProperties.put(selection.getTitle(), mark);
      }
    }
    Configuration configuration = Configuration.getConfiguration();
    configuration.setSubProperties(
        getWikipedia(),
        Configuration.PROPERTIES_BACKLINKS, getPageName(),
        backlinksProperties);
    listLinks.repaint();
  }

  /**
   * Action called when Select next links button is pressed. 
   */
  public void actionSelectNextLinks() {

    // Test
    if (listLinks.getModel().getSize() == 0) {
      return;
    }

    // Check if something can be selected after current selection
    int lastSelected = listLinks.getMaxSelectionIndex();
    int currentLine = lastSelected + 1;
    boolean lineAvailable = false;
    while ((!lineAvailable) &&
           (currentLine < listLinks.getModel().getSize())) {
      Object value = listLinks.getModel().getElementAt(currentLine);
      String property = backlinksProperties.getProperty(value.toString());
      if ((!Configuration.VALUE_PAGE_NORMAL.equals(property)) &&
          (!Configuration.VALUE_PAGE_HELP_NEEDED.equals(property))) {
        lineAvailable = true;
      }
      currentLine++;
    }
    if (!lineAvailable) {
      lastSelected = -1;
    }

    // Find first item to be selected
    currentLine = lastSelected + 1;
    int firstSelection = -1;
    Integer firstNamespace = null;
    while ((firstSelection < 0) &&
           (currentLine < listLinks.getModel().getSize())) {
      Object value = listLinks.getModel().getElementAt(currentLine);
      String property = backlinksProperties.getProperty(value.toString());
      if ((!Configuration.VALUE_PAGE_NORMAL.equals(property)) &&
          (!Configuration.VALUE_PAGE_HELP_NEEDED.equals(property))) {
        firstSelection = currentLine;
        if (value instanceof Page) {
          Page firstPage = (Page) value;
          firstNamespace = firstPage.getNamespace();
        }
      }
      currentLine++;
    }
    if (firstSelection < 0) {
      listLinks.clearSelection();
      listLinks.ensureIndexIsVisible(0);
      return;
    }

    // Initialize array for items to be selected
    Configuration config = Configuration.getConfiguration();
    int maxCount = config.getInt(
        null,
        ConfigurationValueInteger.MAXIMUM_PAGES);
    maxCount = Math.min(maxCount, listLinks.getModel().getSize() - firstSelection);
    if (maxCount <= 0) {
      listLinks.clearSelection();
      listLinks.ensureIndexIsVisible(0);
      return;
    }
    int indices[] = new int[maxCount];

    // Find items to be selected
    currentLine = firstSelection;
    int currentIndice = 0;
    while ((currentIndice < maxCount) &&
           (currentLine < listLinks.getModel().getSize())) {
      Object value = listLinks.getModel().getElementAt(currentLine);
      String property = backlinksProperties.getProperty(value.toString());
      if ((firstNamespace == null) ||
          ((value instanceof Page) &&
           (firstNamespace.equals(((Page) value).getNamespace())))) {
        if ((!Configuration.VALUE_PAGE_NORMAL.equals(property)) &&
            (!Configuration.VALUE_PAGE_HELP_NEEDED.equals(property))) {
          indices[currentIndice] = currentLine;
          currentIndice++;
        }
        currentLine++;
      } else {
        currentLine = Integer.MAX_VALUE;
      }
    }

    // Select items found
    if (currentIndice < indices.length) {
      int newIndices[] = new int[currentIndice];
      for (int i = 0; i < currentIndice; i++) {
        newIndices[i] = indices[i];
      }
      indices = newIndices;
    }
    listLinks.setSelectedIndices(indices);
    if (indices.length > 0) {
      listLinks.ensureIndexIsVisible(indices[0]);
      listLinks.ensureIndexIsVisible(indices[indices.length - 1]);
    }
  }
}
