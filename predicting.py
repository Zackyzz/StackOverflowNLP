from sklearn.model_selection import train_test_split
from sklearn import metrics

datas=['g2fnormal.txt','g2ftitle.txt','g2ftags.txt','g2fwords.txt']

for dataset in datas:
    with open(dataset) as f:
        data=f.read().splitlines()

    dimension = int(data[0])

    splitted = list(map(lambda x: x.split('#',1), data[1:]))

    y = list(map(lambda x: x[1].split()[0], splitted))

    x_prime = list(map(lambda x: x[0].split(),splitted))

    x=[]
    for i in x_prime:
        temp=[0]*dimension
        for j in i:
            splix=j.split(':')
            temp[int(splix[0])]=int(splix[1])
            s = sum(temp)
        x.append([x / s for x in temp])

    X_train, X_test, y_train, y_test = train_test_split(x, y, test_size=0.3)


    print('train')
    from sklearn import svm
    svc = svm.SVC(kernel='rbf', C=5, gamma=1)
    svc.fit(X_train, y_train)
    print('test')
    predictions=svc.predict(X_test)

    print(dataset)
    print(metrics.accuracy_score(y_test,predictions))
    print(metrics.classification_report(y_test,predictions))
    print("finished")

    
    from sklearn.svm import LinearSVC
    lsvc = LinearSVC()
    lsvc.fit(X_train, y_train)

    predictionsl=lsvc.predict(X_test)

    print(metrics.accuracy_score(y_test,predictionsl))
    #print(metrics.classification_report(y_test,predictionsl))


    from sklearn.neighbors import KNeighborsClassifier
    knn = KNeighborsClassifier(n_neighbors=3)

    knn.fit(X_train,y_train)
    pred_knn=knn.predict(X_test)

    print(metrics.accuracy_score(y_test,pred_knn))
    #print(metrics.classification_report(y_test,pred_knn))


    from sklearn import tree
    dt = tree.DecisionTreeClassifier()
    dt.fit(X_train,y_train)

    pred_dt=dt.predict(X_test)

    print(metrics.accuracy_score(y_test,pred_dt))
    #print(metrics.classification_report(y_test,pred_dt))
    