(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  PythonEngine, FMX.PythonGUIInputOutput, MatplotLib, PyCommon, PyModule,
  PyPackage, ScikitLearn, System.Generics.Collections;

type
  TForm2 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo1: TMemo;
    Layout1: TLayout;
    Button1: TButton;
    sklearn: TScikitLearn;
    MatplotLib1: TMatplotLib;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  VarPyth, PyUtils;

procedure TForm2.Button1Click(Sender: TObject);
begin
  //ref: https://scikit-learn.org/stable/auto_examples/applications/plot_face_recognition.html#sphx-glr-auto-examples-applications-plot-face-recognition-py
  var bm := BuiltinModule;
  //See more: https://docs.python.org/3/library/operator.html
  var op := Import('operator'); //provide functions corresponding to the intrinsic operators of Python.
  var logging := Import('logging'); //import loggin
  var time := Import('time'); //import time
  var sklearn_datasets := sklearn.datasets; //from sklearn import datasets
  var sklearn_model_selection := sklearn.model_selection; //from sklearn import model_selection
  var sklearn_decomposition := sklearn.decomposition; //from sklearn import decomposition
  var sklearn_svm := sklearn.svm; //from sklearn import svm
  var sklearn_metrics := sklearn.metrics; //from sklearn import metrics
  var plt := MatplotLib1.plt; //from matplotlib import pyplot

  //Display progress logs on stdout
  logging.basicConfig(level := logging.INFO, format := '%(asctime)s %(message)s');

  //##############################################################################
  //Download the data, if not already on disk and load it as numpy arrays

  var lfw_people := sklearn_datasets.fetch_lfw_people(min_faces_per_person := 70, resize := 0.4);

  //introspect the images arrays to findo the shapes (for plotting)
  var m_result := lfw_people.images.shape;
  var n_samples := m_result.GetItem(0);
  var h := m_result.GetItem(1);
  var w := m_result.GetItem(2);

  // for machine learning we use the 2 data directly (as relative pixel
  // positions info is ignored by this model)
  var X := lfw_people.data;
  var n_features := X.shape[1];

  //the label to predict is the id of the person
  var y := lfw_people.target;
  var target_names := lfw_people.target_names;
  var n_classes := target_names.shape[0];

  bm.print('Total dataset size:');
  bm.print(op.mod('n_samples: %d', n_samples)); // print(%d, % val)
  bm.print(op.mod('n_features: %d', n_features)); // print(%d, % val)
  bm.print(op.mod('n_classes: %d', n_classes)); // print(%d, % val)

  //#############################################################################
  //Split into a training set and a test set using a stratified k fold

  //split into a training and testing set
  m_result := sklearn_model_selection.train_test_split(X, y, test_size := 0.25, random_state := 42);
  var X_train := m_result.GetItem(0);
  var X_test := m_result.GetItem(1);
  var y_train := m_result.GetItem(2);
  var y_test := m_result.GetItem(3);

  //#############################################################################
  //Compute a PCA (eigenfaces) on the face dataset (treated as unlabeled
  //dataset): unsupervised feature extraction / dimensionality reduction
  var n_components := 150;

  bm.print(
    op.mod('Extracting the top %d eigenfaces from %d faces',
      VarPythonCreate([n_components, X_train.shape[0]], stTuple))); // print(%d, % val)

  var t0 := time.time();
  var pca := sklearn_decomposition.PCA(n_components := n_components, svd_solver := 'randomized', whiten := True).fit(X_train);
  bm.print(
    op.mod('done in %0.3fs', VarPythonCreate([
      op.sub(time.time(), t0){a - b}], stTuple))); // print(%d, % val)

  var eigenfaces := pca.components_.reshape(TPyEx.Tuple([n_components, h, w]));

  bm.print('Projecting the input data on the eigenfaces orthonormal basis');
  t0 := time.time();
  var X_train_pca := pca.transform(X_train);
  var X_test_pca := pca.transform(X_test);
  bm.print(op.mod('done in %0.3fs', TPyEx.Tuple([op.sub(time.time(), t0)]))); // print(%d, % (val1 - val2))

  //#############################################################################
  //Train a SVM classification model

  bm.print('Fitting the classifier to the training set');
  t0 := time.time();
  var param_grid := TPyEx.Dictionary([
    TPair<variant, variant>.Create('C', TPyEx.List([1e3, 5e3, 1e4, 5e4, 1e5])),
    TPair<variant, variant>.Create('gamma', TPyEx.List([0.0001, 0.0005, 0.001, 0.005, 0.01, 0.]))
  ]);

  var clf := sklearn_model_selection.GridSearchCV(sklearn_svm.SVC(kernel := 'rbf', class_weight := 'balanced'), param_grid);
  clf := clf.fit(X_train_pca, y_train);
  bm.print(op.mod('done in %0.3fs', (time.time() - t0)));
  bm.print('Best estimator found by grid search:');
  bm.print(clf.best_estimator_);

  //#############################################################################
  //Quantitative evaluation of the model quality on the test set

  bm.print('Predicting people''s names on the test set');
  t0 := time.time();
  var y_pred := clf.predict(X_test_pca);
  bm.print(op.mod('done in %0.3fs', (time.time() - t0)));

  bm.print(sklearn_metrics.classification_report(y_test, y_pred, target_names := target_names));
  bm.print(sklearn_metrics.confusion_matrix(y_test, y_pred, labels := bm.range(n_classes)));

  //#############################################################################
  //Qualitative evaluation of the predictions using matplotlib

  var plot_gallery := procedure(images, titles, hh, ww: variant; const n_row: integer = 3; const n_col: integer = 4)
  begin
    //Helper function to plot a gallery of portraits
    plt.figure(figsize := TPyEx.Tuple([op.mul(1.8, n_col), op.mul(2.4, n_row)]));
    plt.subplots_adjust(bottom := 0, left := 0.01, right := 0.99, top := 0.90, hspace := 0.35);
    for var i in bm.range(n_row * n_col).GetEnumerator() do begin
      plt.subplot(n_row, n_col, i + 1);
      plt.imshow(images.GetItem(i).reshape(TPyEx.Tuple([hh, ww])), cmap := plt.cm.gray);
      plt.title(titles.GetItem(i), size := 12);
      plt.xticks(VarArrayOf([]).AsTuple());
      plt.yticks(VarArrayOf([]).AsTuple());
    end;  
  end;

  //plot the result of the prediction on a portion of the test set

  var title := function(y_pred, y_test, target_names, i: variant): variant
  begin
    var pred_name := target_names.GetItem(y_pred.GetItem(i)).rsplit(' ', 1).GetItem(-1);
    var true_name := target_names.GetItem(y_test.GetItem(i)).rsplit(' ', 1).GetItem(-1);
    Result := op.mod('predicted: %s'+ #10 + 'true:      %s', TPyEx.Tuple([pred_name, true_name]));
  end;

  var prediction_titles := TPyEx.List([]);
  for var i in bm.range(y_pred.shape[0]).GetEnumerator() do begin
    prediction_titles.append(title(y_pred, y_test, target_names, i));   
  end;

  plot_gallery(X_test, prediction_titles, h, w);

  //plot the gallery of the most significative eigenfaces

  var eigenface_titles := TPyEx.List([]);
  for var i in bm.range(eigenfaces.shape[0]).GetEnumerator() do begin
    eigenface_titles.append(op.mod('eigenface %d', i));
  end;

  plot_gallery(eigenfaces, eigenface_titles, h, w);

  plt.show();
end;

end.
