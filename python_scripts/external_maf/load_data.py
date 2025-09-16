import numpy as np
import matplotlib.pyplot as plt

import external_maf.datasets as datasets
import external_maf.util as util


class DATA:

    class Data:

        def __init__(self, data):

            self.x = data.astype(np.float32)
            self.N = self.x.shape[0]

    def __init__(self,path,path1):

        file = datasets.root + path
        file1 = datasets.root + path1
        trn, val, tst, full_data, tst_data = load_data_normalised(file,file1)

        self.trn = self.Data(trn)
        self.val = self.Data(val)
        self.tst = self.Data(tst)
        self.full_data = self.Data(full_data)
        self.tst_data = self.Data(tst_data)
        self.n_dims = self.trn.x.shape[1]

    def show_histograms(self, split, vars):

        data_split = getattr(self, split, None)
        if data_split is None:
            raise ValueError('Invalid data split')

        util.plot_hist_marginals(data_split.x[:, vars])
        plt.show()


def load_data(root_path,root_path1):
    # NOTE: To remember how the pre-processing was done.
    # data = pd.read_csv(root_path, names=[str(x) for x in range(50)], delim_whitespace=True)
    # print data.head()
    # data = data.as_matrix()
    # # Remove some random outliers
    # indices = (data[:, 0] < -100)
    # data = data[~indices]
    #
    # i = 0
    # # Remove any features that have too many re-occuring real values.
    # features_to_remove = []
    # for feature in data.T:
    #     c = Counter(feature)
    #     max_count = np.array([v for k, v in sorted(c.iteritems())])[0]
    #     if max_count > 5:
    #         features_to_remove.append(i)
    #     i += 1
    # data = data[:, np.array([i for i in range(data.shape[1]) if i not in features_to_remove])]
    # np.save("~/data/miniboone/data.npy", data)

    data = np.load(root_path)
    data_f = np.load(root_path)
    data_f_test = np.load(root_path1)
    N_test = int(0.1*data.shape[0])
    data_test = data[-N_test:]
#     data_test = data[-N_test:].reshape(N_test,1)
    data1 = data[0:-N_test]
    N_validate = int(0.05*data1.shape[0])
#     data_validate = data[-N_validate:].reshape(N_validate,1)
    data_validate = data1[-N_validate:]
#     data_train = data[0:-N_validate].reshape(40500,1)
    data_train = data1[0:-N_validate]
    print("dataset shapes : ",data_train.shape, data_validate.shape,data_test.shape) 

    return data_train, data_validate, data_test, data_f, data_f_test


def load_data_normalised(root_path,root_path1):

    data_train, data_validate, data_test, data_full, data_f_test = load_data(root_path,root_path1)
    data = np.vstack((data_train, data_validate))
    mu = data.mean(axis=0)
    s = data.std(axis=0)
#     data_train = (data_train - mu)/s
#     data_validate = (data_validate - mu)/s
#     data_test = (data_test - mu)/s

    return data_train, data_validate, data_test, data_full, data_f_test
