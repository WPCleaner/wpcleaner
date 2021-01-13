/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.BasicPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.PageListAnalyzeListener;
import org.wikipediacleaner.gui.swing.component.PageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.i18n.GT;


/**
 * List of templates linking to a disambiguation page window.
 */
public class TemplateListWindow extends BasicWindow {

  Page page;
  Page link;

  JList<Page> listLinks;
  PageListModel modelLinks;

  /**
   * Create and display a TemplateListWindow.
   * 
   * @param page Initial page.
   * @param link Link.
   * @param wikipedia Wikipedia.
   */
  public static void createTemplateListWindow(
      final Page page,
      final Page link,
      final EnumWikipedia wikipedia) {
    createWindow(
        "TemplateListWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        TemplateListWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof TemplateListWindow) {
              TemplateListWindow templates = (TemplateListWindow) window;
              templates.page = page;
              templates.link = link;
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof TemplateListWindow) {
              TemplateListWindow templates = (TemplateListWindow) window;
              templates.actionReload();
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._T("Templates in {0}", link.getTitle());
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
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Label
    JLabel label = Utilities.createJLabel(GT._T(
        "Templates used in {0}, linking to {1}",
        new Object[] { page.getTitle(), link.getTitle() }));
    panel.add(label, constraints);
    constraints.gridy++;

    // Menu
    modelLinks = new PageListModel();
    modelLinks.setShowDisambiguation(true);
    modelLinks.setShowOther(true);

    // Links
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    listLinks = new JList<>(modelLinks);
    listLinks.setCellRenderer(new PageListCellRenderer());
    listLinks.addMouseListener(new BasicPageListPopupListener(getWikipedia(), null, listLinks, this));
    listLinks.addMouseListener(new PageListAnalyzeListener(getWikipedia(), null));
    JScrollPane scrollLinks = new JScrollPane(listLinks);
    scrollLinks.setMinimumSize(new Dimension(100, 100));
    scrollLinks.setPreferredSize(new Dimension(200, 500));
    scrollLinks.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollLinks, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * Action called when Reload button is pressed. 
   */
  void actionReload() {
    new ReloadWorker(getWikipedia(), this, page, link).start();
  }

  /**
   * Add a link.
   * 
   * @param element Link. 
   */
  void addLink(Object element) {
    modelLinks.addElement(element);
  }

  /**
   * SwingWorker for reloading the page. 
   */
  class ReloadWorker extends BasicWorker {

    private final Page page1;
    private final Page link1;
    private final List<Page> pages;

    /**
     * @param wikipedia Wikipedia.
     * @param window Window.
     * @param page Page.
     * @param link Link.
     */
    public ReloadWorker(EnumWikipedia wikipedia, BasicWindow window, Page page, Page link) {
      super(wikipedia, window);
      this.page1 = page;
      this.link1 = link;
      this.pages = new ArrayList<>();
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#start()
     */
    @Override
    public void start() {
      modelLinks.setShowDisambiguation(true);
      modelLinks.setShowOther(true);
      super.start();
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#finished()
     */
    @Override
    public void finished() {
      super.finished();
      for (Page p : pages) {
        addLink(p);
      }
      if (modelLinks.getSize() > 0) {
        listLinks.setSelectedIndex(0);
      }
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.gui.swing.utils.SwingWorker#construct()
     */
    @Override
    public Object construct() {
      try {
        setText(GT._T("Retrieving MediaWiki API"));
        API api = APIFactory.getAPI();
        setText(GT._T("Retrieving templates"));
        api.retrieveTemplates(getWikipedia(), page1);
        setText(GT._T("Retrieving links in templates"));
        api.retrieveLinks(getWikipedia(), page1.getTemplates());
        setText(GT._T("Displaying templates found"));
        for (Page p : page1.getTemplates()) {
          boolean found = false;
          for (Page l : p.getLinks()) {
            if (link1.getTitle().equals(l.getTitle())) {
              found = true;
              break;
            }
          }
          if (found) {
            pages.add(p);
          }
        }
      } catch (APIException e) {
        return e;
      }
      return null;
    }
  }
}
