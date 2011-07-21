/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing.options;

import java.awt.LayoutManager;
import java.util.HashMap;
import java.util.Map.Entry;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Base class for an Options panel.
 */
abstract class OptionsPanel extends JPanel {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = -6581007549837818171L;

  /**
   * Construct an Options panel.
   * 
   * @param layout Layout manager.
   */
  public OptionsPanel(LayoutManager layout) {
    super(layout);
    booleanValues = new HashMap<ConfigurationValueBoolean, JCheckBox>();
    integerValues = new HashMap<ConfigurationValueInteger, JSpinner>();
    stringValues = new HashMap<ConfigurationValueString, JTextField>();
  }

  // ==========================================================================
  // Actions management
  // ==========================================================================

  /**
   * Restore all options to their default values.
   */
  public void defaultValues() {
    defaultValuesBoolean();
    defaultValuesInteger();
    defaultValuesString();
  }

  /**
   * Apply new values to the options.
   */
  public void apply() {
    applyBoolean();
    applyInteger();
    applyString();
  }

  // ==========================================================================
  // Boolean options management
  // ==========================================================================

  /**
   * Map of check box for each boolean property.
   */
  private final HashMap<ConfigurationValueBoolean, JCheckBox> booleanValues;

  /**
   * @param message Message displayed in the JCheckBox.
   * @param property Boolean property.
   * @return JCheckBox for the boolean property.
   */
  protected JCheckBox createJCheckBox(
      String message,
      ConfigurationValueBoolean property) {
    if (property == null) {
      return null;
    }
    Configuration config = Configuration.getConfiguration();
    boolean selected = config.getBoolean(null, property);
    JCheckBox chk = Utilities.createJCheckBox(message, selected);
    booleanValues.put(property, chk);
    return chk;
  }

  /**
   * Restore all boolean options to their default values.
   */
  private void defaultValuesBoolean() {
    for (Entry<ConfigurationValueBoolean, JCheckBox> entry : booleanValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        entry.getValue().setSelected(entry.getKey().getDefaultValue());
      }
    }
  }

  /**
   * Apply new values to the boolean options.
   */
  private void applyBoolean() {
    Configuration config = Configuration.getConfiguration();

    for (Entry<ConfigurationValueBoolean, JCheckBox> entry : booleanValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        config.setBoolean(null, entry.getKey(), entry.getValue().isSelected());
      }
    }
  }

  // ==========================================================================
  // Integer options management
  // ==========================================================================

  /**
   * Map of spinner for each integer property.
   */
  private final HashMap<ConfigurationValueInteger, JSpinner> integerValues;

  /**
   * @param property Integer property.
   * @param minimum Minimum value.
   * @param maximum Maximum value.
   * @param stepSize Step size.
   * @return JSpinner for the integer property.
   */
  protected JSpinner createJSpinner(
      ConfigurationValueInteger property,
      int minimum, int maximum, int stepSize) {
    if (property == null) {
      return null;
    }
    Configuration config = Configuration.getConfiguration();
    int value = config.getInt(null, property);
    SpinnerNumberModel model = new SpinnerNumberModel(value, minimum, maximum, stepSize);
    JSpinner spin = new JSpinner(model);
    integerValues.put(property, spin);
    return spin;
  }

  /**
   * Restore all integer options to their default values.
   */
  private void defaultValuesInteger() {
    for (Entry<ConfigurationValueInteger, JSpinner> entry : integerValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        SpinnerModel model = entry.getValue().getModel();
        model.setValue(Integer.valueOf(entry.getKey().getDefaultValue()));
      }
    }
  }

  /**
   * Apply new values to the integer options.
   */
  private void applyInteger() {
    Configuration config = Configuration.getConfiguration();

    for (Entry<ConfigurationValueInteger, JSpinner> entry : integerValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        Object value = entry.getValue().getValue();
        if (value instanceof Integer) {
          Integer intValue = (Integer) value;
          config.setInt(null, entry.getKey(), intValue.intValue());
        }
      }
    }
  }

  // ==========================================================================
  // String options management
  // ==========================================================================

  /**
   * Map of text field for each string property.
   */
  private final HashMap<ConfigurationValueString, JTextField> stringValues;

  /**
   * @param property String property.
   * @param columns Number of columns in the text field.
   * @return JTextField for the string property.
   */
  protected JTextField createJTextField(
      ConfigurationValueString property,
      int columns) {
    if (property == null) {
      return null;
    }
    Configuration config = Configuration.getConfiguration();
    String value = config.getString(null, property);
    JTextField txt = new JTextField(columns);
    txt.setText(value);
    stringValues.put(property, txt);
    return txt;
  }

  /**
   * Restore all string options to their default values.
   */
  private void defaultValuesString() {
    for (Entry<ConfigurationValueString, JTextField> entry : stringValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        entry.getValue().setText(entry.getKey().getDefaultValue());
      }
    }
  }

  /**
   * Apply new values to the string options.
   */
  private void applyString() {
    Configuration config = Configuration.getConfiguration();

    for (Entry<ConfigurationValueString, JTextField> entry : stringValues.entrySet()) {
      if ((entry.getValue() != null) && (entry.getKey() != null)) {
        config.setString(null, entry.getKey(), entry.getValue().getText());
      }
    }
  }
}
